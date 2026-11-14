#!/usr/bin/env python3
"""Import trait definitions from austraits.build config/traits.yml into the APD.

Subcommands (run from the APD repo root):

  fetch-try   Download TRY's public trait list (id + name) to <work>/try_traits.csv
  prepare     Diff traits.yml against data/, derive created/modified dates from
              austraits.build git history, propose URIs, and write candidate
              ontology/TRY matches for the new traits. Writes only to <work>.
  review      Write the curator's workbook (accept/reject columns) to <work>
  apply       Write the import into data/: data/edit/APD_traits_input.csv (then run
              `make import-csv`), data/APD_categorical_values_input.csv, and new rows
              in data/published_classes.csv and data/APD_units.csv. Also writes the
              review tables to <work>.

Curation lives in two CSVs in <work> that `prepare` seeds and a person (or agent)
edits before `apply`:

  uris.csv     trait, identifier, basis           -- one row per new trait
  matches.csv  trait, exact_match, close_match, related_match,
               TRY_exact, TRY_close, TRY_related, constraints
               (TRY_* hold TRY trait ids, '; '-separated; names are looked up)

Needs python3 with PyYAML. Reads austraits.build via `git -C`, never writes to it.
"""
import argparse, collections, csv, datetime as dt, html, io, json, os, re, subprocess, sys, urllib.request, urllib.parse
from decimal import Decimal

import yaml

DATA = 'data'
TRY_URL = 'https://www.try-db.org/de/TabDetails.php'
TRY_SUFFIX = '(https://www.try-db.org/de/de.php)'
BIEN_SUFFIX = '[BIEN] (https://bien.nceas.ucsb.edu/bien/biendata)'
TYPE = {'numeric': 'STATO_0000251', 'categorical': 'STATO_0000252', 'monthly_presence': 'STATO_0000252'}
# Fields austraits.build owns; everything else in the APD record (dates, reviewers,
# references, matches, constraints, deprecated names) is APD-only and is preserved.
AB_OWNED = ['label', 'description_encoded', 'description', 'comments', 'type', 'min', 'max',
            'units', 'units_uom', 'category', 'structure', 'measured_characteristic', 'keywords']
SIGNATURE = [f for f in AB_OWNED if f not in ('type', 'units_uom')]  # what counts as "modified"
MATCH_COLS = ['exact_match', 'close_match', 'related_match', 'TRY_exact', 'TRY_close', 'TRY_related',
              'BIEN_exact', 'BIEN_close', 'BIEN_related', 'constraints',
              'category']  # category: optional override, trait_group ids


def ws(s):
    return ' '.join(str(s).split()) if s is not None else None


def read_csv(path, encoding='utf-8-sig'):
    with open(path, encoding=encoding, newline='') as f:
        return list(csv.DictReader(f))


def write_csv(path, rows, fields):
    with open(path, 'w', encoding='utf-8', newline='') as f:
        w = csv.DictWriter(f, fieldnames=fields, lineterminator='\n', extrasaction='ignore')
        w.writeheader()
        w.writerows(rows)


def fmtnum(x):
    """Plain decimal text, never scientific: 1e-05 -> 0.00001, 15000.0 -> 15000."""
    if x is None:
        return None
    s = format(Decimal(repr(x)) if isinstance(x, float) else Decimal(str(x)), 'f')
    if '.' in s:
        s = s.rstrip('0').rstrip('.')
    return s


# --- the APD's lookup tables ---------------------------------------------------

class Vocab:
    def __init__(self, apd_traits, work=None):
        pc = read_csv(os.path.join(DATA, 'published_classes.csv'))
        gl = read_csv(os.path.join(DATA, 'APD_glossary.csv'))
        self.hier = read_csv(os.path.join(DATA, 'APD_trait_hierarchy.csv'))
        units = read_csv(os.path.join(DATA, 'APD_units.csv'))
        self.lab2id = collections.defaultdict(list)
        for r in pc + gl:
            self.lab2id[ws(r['label']).lower()].append(r['identifier'].strip())
        self.known_ids = set(r['identifier'].strip() for r in pc + gl)
        self.grp2id = {ws(r['label']).lower(): r['identifier'] for r in self.hier}
        self.grp_label = {r['identifier']: ws(r['label']) for r in self.hier}
        self.parent = {r['identifier']: r['Parent'].rsplit('/', 1)[-1] for r in self.hier}
        # unit -> uom label: what the APD already uses, else APD_units.csv codes
        self.uom = {}
        use = collections.defaultdict(collections.Counter)
        for v in apd_traits.values():
            if v.get('units') and v.get('units_uom'):
                use[v['units']][v['units_uom']] += 1
        for r in units:
            for c in ('identifier', 'SI_code', 'UCUM_code', 'UCUM_code_02', 'UCUM_code_03', 'UCUM_code_04'):
                x = (r.get(c) or '').strip()
                if x:
                    self.uom.setdefault(x, r['label'].strip())
        for u, c in use.items():
            self.uom[u] = c.most_common(1)[0][0]
        # per-release overrides for units the APD has never used: <work>/units_uom.csv (unit, units_uom)
        if work and os.path.exists(os.path.join(work, 'units_uom.csv')):
            for r in read_csv(os.path.join(work, 'units_uom.csv')):
                self.uom[r['unit']] = r['units_uom']
        self.uom_labels = set(r['label'].strip() for r in units)
        # how often each id is used, per field -- to break ties between same-label terms
        self.use = collections.defaultdict(collections.Counter)
        for v in apd_traits.values():
            for f in ('structure', 'measured_characteristic', 'keywords'):
                for x in str(v.get(f) or '').split('; '):
                    self.use[f][x] += 1

    def ancestors(self, gid):
        out = set()
        p = self.parent.get(gid)
        while p and p.startswith('trait_group'):
            out.add(p)
            p = self.parent.get(p)
        return out

    def labels_to_ids(self, s, field, prefer=()):
        """'; '-joined labels -> ids. Labels may themselves contain '; ' (e.g.
        'ratio; proportion'), so match greedily, longest first."""
        if s is None:
            return None, [], []
        parts = [p.strip() for p in str(s).split(';')]
        ids, bad, amb = [], [], []
        i = 0
        while i < len(parts):
            for j in range(len(parts), i, -1):
                cand = ws('; '.join(parts[i:j])).lower()
                if cand in self.lab2id:
                    opts = list(dict.fromkeys(self.lab2id[cand]))
                    opts.sort(key=lambda x: (x not in prefer, -self.use[field][x], x))
                    ids.append(opts[0])
                    if len(opts) > 1:
                        amb.append(f'{cand} -> {opts[0]} (of {", ".join(opts)})')
                    i = j
                    break
            else:
                if parts[i]:
                    bad.append(parts[i])
                i += 1
        return '; '.join(dict.fromkeys(ids)) or None, bad, amb

    def groups_to_category(self, s):
        gids, bad = [], []
        for g in str(s).split(';'):
            g = ws(g).lower()
            if not g:
                continue
            (gids if g in self.grp2id else bad).append(self.grp2id.get(g, g))
        gids = [g for g in gids if g in self.grp_label]
        anc = set().union(*[self.ancestors(g) for g in gids]) if gids else set()
        leaves = [g for g in gids if g not in anc]
        return gids, leaves, bad


# --- description handling --------------------------------------------------------

TAG_UNDERSCORE = re.compile(r'\[([A-Za-z]+)_(\d{5,})\]')        # [PO_0025082] -> [PO:0025082]
TAG_NO_OPEN = re.compile(r'(?<![\[\w:])([A-Z][A-Za-z]+:\d{5,})\]')  # 'PATO:0000119]' -> '[PATO:0000119]'
TAG_NO_SPACE = re.compile(r'(\w)\[([A-Za-z_]+[:_][^\]]+)\]')     # 'taxon[NCIT:..]' -> 'taxon [NCIT:..]'
TAGS = re.compile(r' ?\[[^\]]*\]')


def clean_encoded(s):
    fixes = []

    def open_bracket(m):  # only outside an existing [...], e.g. not in '[ENVO:01000451; CHEBI:16526]'
        before = m.string[:m.start()]
        return m.group(0) if before.count('[') > before.count(']') else f'[{m.group(1)}]'

    for rx, rep, what in ((TAG_UNDERSCORE, r'[\1:\2]', 'underscore in tag'),
                          (TAG_NO_OPEN, open_bracket, 'tag missing ['),
                          (TAG_NO_SPACE, r'\1 [\2]', 'no space before tag')):
        s2 = rx.sub(rep, s)
        if s2 != s:
            fixes.append(what)
            s = s2
    if s.count('[') != s.count(']'):
        fixes.append('UNBALANCED brackets - check by hand')
    return s, fixes


def split_description(d):
    """traits.yml packs 'encoded;plain' into one field. The joiner is a ';' with no
    space after it, at bracket depth 0 -- a '; ' inside either half is prose."""
    d = ws(d) or ''
    depth, cands = 0, []
    for k, ch in enumerate(d):
        depth += (ch == '[') - (ch == ']')
        if ch == ';' and depth <= 0 and k + 1 < len(d) and d[k + 1] != ' ':
            cands.append(k)
    if not cands:  # fall back to '.; Capital'
        cands = [m.start() + 1 for m in re.finditer(r'\.; (?=[A-Z])', d)]
    if cands:
        k = cands[-1]
        return d[:k].strip(), d[k + 1:].strip(), None
    return d, None, 'no plain-text description; derived by stripping tags'


# --- reading traits.yml ----------------------------------------------------------

def load_ab(text, work=None):
    """traits.yml's trait list, with this release's APD-side overrides applied:
    <work>/renames.csv (old,new) for a trait renamed in the APD ahead of austraits.build,
    <work>/drop_values.csv (trait,value) for an allowable value the APD drops."""
    ab = yaml.safe_load(text)['traits']['elements']
    if not work:
        return ab
    rp, dp = os.path.join(work, 'renames.csv'), os.path.join(work, 'drop_values.csv')
    ren = {r['old']: r['new'] for r in read_csv(rp)} if os.path.exists(rp) else {}
    if ren:
        ab = {ren.get(k, k): v for k, v in ab.items()}
    for r in (read_csv(dp) if os.path.exists(dp) else []):
        (ab.get(r['trait'], {}).get('allowed_values_levels') or {}).pop(r['value'], None)
    return ab


def convert(name, v, vocab, existing=None):
    """One traits.yml entry -> APD fields it owns, plus a list of notes."""
    notes = []
    out = {'trait': name, 'label': ws(v.get('label'))}
    enc, plain, why = split_description(v.get('description'))
    enc, fixes = clean_encoded(enc)
    notes += [f'description: {f}' for f in fixes]
    if why:
        notes.append(why)
        plain = ws(TAGS.sub('', enc))
    out['description_encoded'], out['description'] = enc, plain
    com = v.get('comments', v.get('comment'))
    out['comments'] = ws(com) if com else None
    out['type'] = TYPE.get(v.get('type'))
    if v.get('type') == 'numeric':
        out['min'], out['max'] = fmtnum(v.get('allowed_values_min')), fmtnum(v.get('allowed_values_max'))
        out['units'] = str(v['units']) if v.get('units') is not None else None
        out['units_uom'] = vocab.uom.get(out['units'])
        if existing and existing.get('units') == out['units'] and existing.get('units_uom'):
            out['units_uom'] = existing['units_uom']  # unit unchanged: keep the APD's label
        if out['units'] and not out['units_uom']:
            notes.append(f'unit {out["units"]} has no APD_units.csv row')
    else:
        out['min'] = out['max'] = out['units'] = out['units_uom'] = None
    prefer = {}
    if existing:
        for f in ('structure', 'measured_characteristic', 'keywords'):
            prefer[f] = set(str(existing.get(f) or '').split('; '))
    for apd_f, ab_f in (('structure', 'structure_measured'), ('measured_characteristic', 'characteristic_measured'),
                        ('keywords', 'keywords')):
        src = v.get(ab_f) if ab_f != 'keywords' else v.get('keywords', v.get('keyswords'))
        out[apd_f], bad, amb = vocab.labels_to_ids(src, apd_f, prefer.get(apd_f, ()))
        notes += [f'{apd_f}: unknown label "{b}"' for b in bad]
        notes += [f'{apd_f}: ambiguous {a}' for a in amb]
    gids, leaves, gbad = vocab.groups_to_category(v.get('trait_group', ''))
    notes += [f'trait_group: unknown "{b}"' for b in gbad]
    out['_groups'] = gids
    if existing and gids and gids[0] in str(existing.get('category', '')).split('; '):
        out['category'] = existing['category']   # traits.yml holds one path; keep APD's extra groups
    else:
        out['category'] = '; '.join(leaves) or None
        if len(leaves) > 1:
            notes.append('category: traits.yml lists groups on two branches -> ' +
                         '; '.join(vocab.grp_label[g] for g in leaves))
    out['_levels'] = {str(k): ws(d) for k, d in (v.get('allowed_values_levels') or {}).items()}
    return out, notes


def signature(rec):
    return json.dumps([rec.get(f) for f in SIGNATURE] + [sorted(rec.get('_levels', {}).items())])


# --- git history -> dates --------------------------------------------------------

def history_dates(ab_repo, since, ref, vocab, work=None):
    """For each trait: the date it first appears after `since`, and the date of the
    last commit that changed what the APD publishes about it."""
    log = subprocess.run(['git', '-C', ab_repo, 'log', '--first-parent', '--reverse', '--format=%H %ad',
                          '--date=short', f'{since}^..{ref}', '--', 'config/traits.yml'],
                         capture_output=True, text=True, check=True).stdout.split('\n')
    commits = [c.split() for c in log if c]
    first, last, prev = {}, {}, {}
    for h, d in commits:
        txt = subprocess.run(['git', '-C', ab_repo, 'show', f'{h}:config/traits.yml'],
                             capture_output=True, text=True, check=True).stdout
        for t, v in load_ab(txt, work).items():
            s = signature(convert(t, v, vocab)[0])
            if t not in first:
                first[t] = d
            if t in prev and prev[t] != s:
                last[t] = d
            prev[t] = s
    return {'base': commits[0][1], 'first': first, 'last': last}


# --- URIs ------------------------------------------------------------------------

def used_identifiers():
    """Every trait number the APD has ever published -- never reuse one."""
    used = set()
    for root, _, files in os.walk('release'):
        for f in files:
            if f.endswith(('.csv', '.ttl', '.nt')):
                with open(os.path.join(root, f), encoding='utf-8', errors='ignore') as fh:
                    used.update(int(x) for x in re.findall(r'trait_(\d{7})\b', fh.read()))
    with open(os.path.join(DATA, 'APD_traits_input.yml'), encoding='utf-8') as fh:
        used.update(int(x) for x in re.findall(r'^trait_(\d{7}):', fh.read(), re.M))
    return used


def propose_uris(ab, apd_by_name, used):
    """Next free number after the preceding existing trait (in traits.yml order).
    'gap' = it fits before the next existing trait; 'NO-GAP' = it spills past it."""
    names, out, taken, prev = list(ab), [], set(used), None
    for i, t in enumerate(names):
        if t in apd_by_name:
            prev = int(apd_by_name[t].split('_')[1])
            continue
        nxt = next((int(apd_by_name[x].split('_')[1]) for x in names[i + 1:] if x in apd_by_name), None)
        c = (prev or 0) + 1
        while c in taken:
            c += 1
        taken.add(c)
        fits = nxt is None or c < nxt
        out.append({'trait': t, 'identifier': f'trait_{c:07d}',
                    'basis': ('gap' if fits else 'NO-GAP') + f': after trait_{prev:07d}' +
                             (f', before trait_{nxt:07d}' if nxt else '')})
    return out


# --- candidates ------------------------------------------------------------------

def parse_obo(path):
    terms, cur = {}, None
    with open(path, encoding='utf-8') as f:
        for line in f:
            line = line.rstrip('\n')
            if line == '[Term]':
                cur = {'syn': []}
                continue
            if line.startswith('['):
                cur = None
                continue
            if cur is None:
                continue
            if line.startswith('id: '):
                cur['id'] = line[4:]
                terms[cur['id']] = cur
            elif line.startswith('name: '):
                cur['name'] = line[6:]
            elif line.startswith('def: '):
                cur['def'] = re.sub(r'^"(.*)" \[.*$', r'\1', line[5:])
            elif line.startswith('synonym: '):
                cur['syn'].append(re.sub(r'^"(.*?)".*', r'\1', line[9:]))
            elif line.startswith('is_obsolete: true'):
                cur['obs'] = True
    return {k: v for k, v in terms.items() if not v.get('obs') and 'name' in v}


STOP = set('a an the of per to in on and or is which that by as with unit trait content plant'.split())
NORM = {'colour': 'color', 'leaves': 'leaf', 'number': 'count', 'fibre': 'fiber'}


def toks(s):
    w = re.findall(r'[a-z0-9]+', TAGS.sub('', s.lower()))
    return set(NORM.get(x, x[:-1] if x.endswith('s') and len(x) > 4 else x) for x in w if x not in STOP)


def jacc(q, names):
    best = 0
    for n in names:
        t = toks(n)
        if t:
            best = max(best, len(q & t) / len(q | t))
    return best


# --- commands --------------------------------------------------------------------

def load_state(args):
    with open(os.path.join(DATA, 'APD_traits_input.yml'), encoding='utf-8') as f:
        apd = yaml.safe_load(f)
    txt = subprocess.run(['git', '-C', args.ab, 'show', f'{args.ref}:config/traits.yml'],
                         capture_output=True, text=True, check=True).stdout
    ab = load_ab(txt, args.work)
    return apd, ab, {v['trait']: k for k, v in apd.items()}, Vocab(apd, args.work)


def cmd_fetch_try(args):
    os.makedirs(args.work, exist_ok=True)
    req = urllib.request.Request(TRY_URL, data=urllib.parse.urlencode({'OutTab': 'tab1', 'submit': 'Go'}).encode(),
                                 headers={'User-Agent': 'Mozilla/5.0'})
    s = urllib.request.urlopen(req, timeout=120).read().decode('latin-1')
    rows = []
    for r in re.findall(r'<tr[^>]*>(.*?)</tr>', s, re.S | re.I):
        cells = [html.unescape(re.sub('<[^>]+>', '', c)).strip() for c in re.findall(r'<td[^>]*>(.*?)</td>', r, re.S | re.I)]
        if cells and cells[0].isdigit():
            rows.append({'TRY_id': cells[0], 'name': cells[1]})
    write_csv(os.path.join(args.work, 'try_traits.csv'), rows, ['TRY_id', 'name'])
    print(f'{len(rows)} TRY traits -> {args.work}/try_traits.csv')


def cmd_prepare(args):
    os.makedirs(args.work, exist_ok=True)
    apd, ab, apd_by_name, vocab = load_state(args)
    hist = history_dates(args.ab, args.since, args.ref, vocab, args.work)
    with open(os.path.join(args.work, 'history.json'), 'w') as f:
        json.dump(hist, f, indent=1)
    new = [t for t in ab if t not in apd_by_name]
    gone = [t for t in apd_by_name if t not in ab]
    cats = collections.defaultdict(dict)
    for r in read_csv(os.path.join(DATA, 'APD_categorical_values_input.csv')):
        cats[r['trait_name']][r['label']] = ws(r['description'])

    changes, notes_rows = [], []
    for t, v in ab.items():
        ex = apd.get(apd_by_name.get(t))
        rec, notes = convert(t, v, vocab, ex)
        for n in notes:
            notes_rows.append({'trait': t, 'new': 'yes' if not ex else '', 'note': n})
        if not ex:
            continue
        for f in AB_OWNED:
            a, b = ws(ex.get(f)) or None, rec.get(f) or None
            if a != b:
                changes.append({'trait': t, 'identifier': apd_by_name[t], 'field': f, 'apd': a, 'traits_yml': b,
                                'last_changed': hist['last'].get(t, '')})
        old, newl = cats.get(t, {}), rec['_levels']
        for k in newl.keys() - old.keys():
            changes.append({'trait': t, 'identifier': apd_by_name[t], 'field': 'level added', 'apd': '', 'traits_yml': k})
        for k in old.keys() - newl.keys():
            changes.append({'trait': t, 'identifier': apd_by_name[t], 'field': 'LEVEL REMOVED (breaking)', 'apd': k, 'traits_yml': ''})
        for k in old.keys() & newl.keys():
            if old[k] != newl[k]:
                changes.append({'trait': t, 'identifier': apd_by_name[t], 'field': f'level description: {k}',
                                'apd': old[k], 'traits_yml': newl[k]})
    # The first run's diff is the record of what this import changes; once `apply` has run,
    # a re-run diffs against the imported state, so it must not overwrite that record.
    def first_or_rerun(name):
        p = os.path.join(args.work, name + '.csv')
        return p if not os.path.exists(p) else os.path.join(args.work, name + '_rerun.csv')
    changes_path, notes_path = first_or_rerun('existing_changes'), first_or_rerun('conversion_notes')
    write_csv(changes_path, changes, ['trait', 'identifier', 'field', 'apd', 'traits_yml', 'last_changed'])
    write_csv(notes_path, notes_rows, ['trait', 'new', 'note'])

    uris_path = os.path.join(args.work, 'uris.csv')
    proposal = propose_uris(ab, apd_by_name, used_identifiers())
    write_csv(os.path.join(args.work, 'uri_proposal.csv'), proposal, ['trait', 'identifier', 'basis'])
    if not os.path.exists(uris_path):
        write_csv(uris_path, proposal, ['trait', 'identifier', 'basis'])

    # candidate matches
    onto = {}
    for fn in ('to.obo', 'po.obo'):
        p = os.path.join(args.ontologies, fn)
        if os.path.exists(p):
            onto.update(parse_obo(p))
    tryp = os.path.join(args.work, 'try_traits.csv')
    tryt = [(r['TRY_id'], r['name'], toks(r['name'])) for r in read_csv(tryp)] if os.path.exists(tryp) else []
    lines = []
    for t in new:
        v = ab[t]
        q = toks(t.replace('_', ' ')) | toks(v['label'])
        oc = sorted(((jacc(q, [x['name']] + x['syn']), i, x['name']) for i, x in onto.items()), reverse=True)[:6]
        tc = sorted(((len(q & tt) / len(q | tt) if tt else 0, i, n) for i, n, tt in tryt), reverse=True)[:6]
        rec, _ = convert(t, v, vocab)
        lines.append(f"## {t} | {rec['label']} | {v['type']}\n  D: {rec['description'][:240]}\n"
                     f"  S: {v.get('structure_measured')} | K: {v.get('keywords')}\n"
                     f"  ONT: " + '; '.join(f'{i} {n} ({s:.2f})' for s, i, n in oc if s > 0.15) + "\n"
                     f"  TRY: " + '; '.join(f'{i} {n} ({s:.2f})' for s, i, n in tc if s > 0.2))
    with open(os.path.join(args.work, 'candidates.txt'), 'w') as f:
        f.write('\n'.join(lines) + '\n')
    mpath = os.path.join(args.work, 'matches.csv')
    if not os.path.exists(mpath):
        write_csv(mpath, [{'trait': t} for t in new], ['trait'] + MATCH_COLS)

    print(f'traits.yml: {len(ab)} traits; APD: {len(apd)}. New: {len(new)}. In APD but not traits.yml: {gone}')
    print(f'Existing-trait changes: {len(changes)} rows over {len(set(c["trait"] for c in changes))} traits '
          f'-> {changes_path}')
    print(f'URI proposal: {sum(p["basis"].startswith("NO-GAP") for p in proposal)} of {len(proposal)} do not fit a gap '
          f'-> {args.work}/uri_proposal.csv (edit {args.work}/uris.csv)')
    print(f'Conversion notes: {len(notes_rows)} -> {notes_path}')
    print(f'Candidates -> {args.work}/candidates.txt; fill in {args.work}/matches.csv')


def cmd_apply(args):
    apd, ab, apd_by_name, vocab = load_state(args)
    with open(os.path.join(args.work, 'history.json')) as f:
        hist = json.load(f)
    uris = {r['trait']: r['identifier'].strip() for r in read_csv(os.path.join(args.work, 'uris.csv'))}
    matches = {r['trait']: r for r in read_csv(os.path.join(args.work, 'matches.csv'))}
    tryp = os.path.join(args.work, 'try_traits.csv')
    tryname = {r['TRY_id']: r['name'] for r in read_csv(tryp)} if os.path.exists(tryp) else {}
    bienp = os.path.join(args.ontologies, 'BIEN_traits.csv')
    bien = {r['trait_name'] for r in read_csv(bienp)} if os.path.exists(bienp) else None
    today = args.today or dt.date.today().isoformat()
    keep = set(args.keep.split(',')) if args.keep else set()
    errors = []
    old_cats = collections.defaultdict(dict)
    for r in read_csv(os.path.join(DATA, 'APD_categorical_values_input.csv'), encoding='utf-8'):
        old_cats[r['trait_name']][r['label']] = ws(r['description'])

    # 1. the trait table, via the spreadsheet checkout
    csv_path = os.path.join(DATA, 'edit', 'APD_traits_input.csv')
    if not os.path.exists(csv_path):
        sys.exit(f'{csv_path} missing: run `make export-csv` first')
    rows = read_csv(csv_path, encoding='utf-8')
    fields = list(rows[0].keys())
    by_id = {r['identifier']: r for r in rows}
    # APD-side renames of traits already in the YAML (renames.csv). A published name is kept
    # as deprecated_trait_name; one that never reached a release is simply replaced.
    rp = os.path.join(args.work, 'renames.csv')
    published = ''
    for root, _, files in os.walk('release'):
        for fn in files:
            if fn == 'APD_traits.csv':
                with open(os.path.join(root, fn), encoding='utf-8', errors='ignore') as fh:
                    published += fh.read()
    for r in (read_csv(rp) if os.path.exists(rp) else []):
        ident = apd_by_name.pop(r['old'], None)
        if ident:
            row = by_id[ident]
            row['trait'] = r['new']
            if re.search(rf'\b{re.escape(r["old"])}\b', published):
                row['deprecated_trait_name'] = '; '.join(x for x in [row.get('deprecated_trait_name'), r['old']] if x)
            apd[ident] = dict(apd[ident], trait=r['new'])
            apd_by_name[r['new']] = ident
    dp = os.path.join(args.work, 'drop_values.csv')
    dropped = {r['trait'] for r in read_csv(dp)} if os.path.exists(dp) else set()
    levels = {}
    review_new, review_try = [], []
    for t, v in ab.items():
        ident = apd_by_name.get(t)
        ex = apd.get(ident)
        rec, notes = convert(t, v, vocab, ex)
        levels[t] = rec['_levels']
        if ex:
            row = by_id[ident]
            changed = any((ws(ex.get(f)) or None) != (rec.get(f) or None) for f in SIGNATURE) \
                or old_cats.get(t, {}) != levels[t]
            for f in AB_OWNED:
                row[f] = rec.get(f) or ''
            if changed:
                row['modified'] = today if t in dropped else (hist['last'].get(t) or today)
        else:
            ident = uris.get(t)
            if not ident or not re.fullmatch(r'trait_\d{7}', ident) or ident in by_id:
                errors.append(f'{t}: bad or duplicate identifier {ident!r} in uris.csv')
                continue
            row = {f: '' for f in fields}
            row.update({f: rec.get(f) or '' for f in AB_OWNED})
            row.update(identifier=ident, trait=t, inScheme='https://w3id.org/APD',
                       created=hist['first'].get(t, today), modified=hist['last'].get(t) or hist['first'].get(t, today))
            rows.append(row)
            by_id[ident] = row
        if t in uris:  # this import's new traits: matches always come from matches.csv
            if uris[t] != ident:
                errors.append(f'{t}: uris.csv says {uris[t]} but the APD already has it as {ident}')
            m = matches.get(t, {})
            for c in MATCH_COLS:
                val = (m.get(c) or '').strip()
                if c == 'category' and not val:
                    continue
                if c.startswith('TRY_') and val:
                    parts = []
                    for i in [x.strip() for x in val.split(';') if x.strip()]:
                        if i not in tryname:
                            errors.append(f'{t}: TRY id {i} not in try_traits.csv')
                        parts.append(f'{tryname.get(i, "?")} [TRY:{i}] {TRY_SUFFIX}')
                        review_try.append({'trait': t, 'label': rec['label'], 'match': c.split('_')[1],
                                           'TRY_id': i, 'TRY_name': tryname.get(i, '?')})
                    val = '; '.join(parts)
                if c.startswith('BIEN_') and val:
                    names = [x.strip() for x in val.split(';') if x.strip()]
                    for n in names:
                        if bien is not None and n not in bien:
                            errors.append(f'{t}: BIEN trait {n!r} not in {bienp}')
                    val = '; '.join(f'{n} {BIEN_SUFFIX}' for n in names)
                row[c] = val
            review_new.append({'identifier': ident, 'trait': t, 'label': rec['label'], 'type': v['type'],
                               'category': '; '.join(vocab.grp_label.get(g, g) for g in (rec['category'] or '').split('; ')),
                               **{c: row.get(c, '') for c in MATCH_COLS}, 'notes': ' | '.join(notes)})
    removed = [t for t in apd_by_name if t not in ab and t not in keep]
    for t in removed:
        rows = [r for r in rows if r['trait'] != t]
    if errors:
        sys.exit('Not written:\n  ' + '\n  '.join(errors))
    with open(csv_path, 'w', encoding='utf-8', newline='') as f:
        w = csv.DictWriter(f, fieldnames=fields, lineterminator='\n')
        w.writeheader()
        w.writerows(sorted(rows, key=lambda r: r['identifier']))

    # 2. categorical values: keep file order, update in place, add after each trait's block
    cpath = os.path.join(DATA, 'APD_categorical_values_input.csv')
    old = read_csv(cpath, encoding='utf-8')
    ren = {r['old']: r['new'] for r in (read_csv(rp) if os.path.exists(rp) else [])}
    for r in old:  # a renamed trait's values are renamed with it: identifier is <trait>_<value>
        if r['trait_name'] in ren:
            r['trait_name'] = ren[r['trait_name']]
            r['identifier'] = f"{r['trait_name']}_{r['label']}"
    out, seen = [], collections.defaultdict(set)
    blocks = collections.OrderedDict()
    for r in old:
        blocks.setdefault(r['trait_name'], []).append(r)
    for t, rs in blocks.items():
        if t in removed:
            continue
        if t not in levels:  # not in traits.yml (kept on purpose): leave alone
            out += rs
            continue
        for r in rs:
            if r['label'] in levels[t]:
                r = dict(r, description=levels[t][r['label']])
                out.append(r)
                seen[t].add(r['label'])
        for k, d in levels[t].items():
            if k not in seen[t]:
                out.append({'identifier': f'{t}_{k}', 'label': k, 'description': d, 'trait_name': t})
    for t in ab:
        if t not in blocks:
            for k, d in levels[t].items():
                out.append({'identifier': f'{t}_{k}', 'label': k, 'description': d, 'trait_name': t})
    write_csv(cpath, out, ['identifier', 'label', 'description', 'trait_name'])

    # 3. borrowed terms the new matches use
    onto = {}
    for fn in ('to.obo', 'po.obo'):
        p = os.path.join(args.ontologies, fn)
        if os.path.exists(p):
            onto.update(parse_obo(p))
    known = set(r['identifier'].strip() for r in read_csv(os.path.join(DATA, 'published_classes.csv')))
    need = set()
    for r in rows:
        for c in ('exact_match', 'close_match', 'related_match'):
            need.update(x for x in (r.get(c) or '').split('; ') if x)
    add, missing = [], []
    for i in sorted(need - known):
        m = re.fullmatch(r'([A-Za-z]+)_(\d+)', i)
        term = onto.get(f'{m.group(1)}:{m.group(2)}') if m else None
        if not term:
            missing.append(i)
            continue
        names = {'TO': 'Plant Trait Ontology', 'PO': 'Plant Ontology'}
        add.append([f'http://purl.obolibrary.org/obo/{i}', term['name'], term.get('def', ''), i,
                    'http://purl.obolibrary.org/obo/', 'obo', m.group(1), names.get(m.group(1), m.group(1))])
    if add:
        buf = io.StringIO()
        csv.writer(buf, lineterminator='\n').writerows(add)
        with open(os.path.join(DATA, 'published_classes.csv'), 'a', encoding='utf-8', newline='') as f:
            f.write(buf.getvalue())

    write_csv(os.path.join(args.work, 'review_new_traits.csv'), review_new,
              ['identifier', 'trait', 'label', 'type', 'category'] + MATCH_COLS + ['notes'])
    write_csv(os.path.join(args.work, 'review_TRY.csv'), review_try, ['trait', 'label', 'match', 'TRY_id', 'TRY_name'])
    units_missing = sorted(set(r['units'] for r in rows if r.get('units') and r.get('units_uom', '') == ''))
    print(f'Wrote {csv_path} ({len(rows)} traits; removed {removed or "none"}). Now run `make import-csv`.')
    print(f'Wrote {cpath} ({len(out)} values).')
    print(f'published_classes.csv: +{len(add)} terms' + (f'; NOT FOUND locally, add by hand: {missing}' if missing else ''))
    if units_missing:
        print(f'Units with no APD_units.csv row (add them, then re-run): {units_missing}')
    print(f'Review tables -> {args.work}/review_new_traits.csv, {args.work}/review_TRY.csv')


def cmd_review(args):
    """The curator's workbook: accept/reject columns on everything a person must confirm."""
    from openpyxl import Workbook
    from openpyxl.styles import Font, PatternFill, Alignment
    from openpyxl.worksheet.datavalidation import DataValidation
    with open(os.path.join(DATA, 'APD_traits_input.yml'), encoding='utf-8') as f:
        apd = yaml.safe_load(f)
    pc = {r['identifier'].strip(): r for r in read_csv(os.path.join(DATA, 'published_classes.csv'))}
    hier = {r['identifier']: ws(r['label']) for r in read_csv(os.path.join(DATA, 'APD_trait_hierarchy.csv'))}
    uris = {r['trait']: r for r in read_csv(os.path.join(args.work, 'uris.csv'))}
    by_name = {v['trait']: (k, v) for k, v in apd.items()}
    order = [t for t in uris if t in by_name]
    wb = Workbook()

    def sheet(name, header, rows, widths, accept=True):
        sh = wb.create_sheet(name)
        h = header + (['accept (y/n)', 'comment'] if accept else [])
        sh.append(h)
        for c in sh[1]:
            c.font, c.fill = Font(bold=True, color='FFFFFF'), PatternFill('solid', fgColor='305496')
        for r in rows:
            sh.append(r)
        for i, w in enumerate(widths + ([10, 30] if accept else []), 1):
            sh.column_dimensions[sh.cell(1, i).column_letter].width = w
        for row in sh.iter_rows():
            for c in row:
                c.alignment = Alignment(wrap_text=True, vertical='top')
        sh.freeze_panes, sh.auto_filter.ref = 'B2', sh.dimensions
        if accept and sh.max_row > 1:
            col = sh.cell(1, len(h) - 1).column_letter
            dv = DataValidation(type='list', formula1='"y,n"', allow_blank=True)
            sh.add_data_validation(dv)
            dv.add(f'{col}2:{col}{sh.max_row}')

    rows = []
    for t in order:
        k, v = by_name[t]
        for f in ('TRY_exact', 'TRY_close', 'TRY_related'):
            for m in re.finditer(r'(.*?) \[TRY:(\d+)\] \(https://www\.try-db\.org/de/de\.php\)(?:; )?', str(v.get(f) or '')):
                rows.append([t, k, v['label'], ws(v['description']), f.split('_')[1], int(m.group(2)), m.group(1)])
    sheet('TRY mappings', ['trait', 'identifier', 'APD label', 'APD description', 'match', 'TRY id', 'TRY trait name'],
          rows, [34, 15, 34, 60, 9, 8, 55])
    rows = []
    for t in order:
        k, v = by_name[t]
        for f in ('BIEN_exact', 'BIEN_close', 'BIEN_related'):
            for n in re.findall(r'(.*?) \[BIEN\] \(https://bien\.nceas\.ucsb\.edu/bien/biendata\)(?:; )?', str(v.get(f) or '')):
                rows.append([t, k, v['label'], ws(v['description']), f.split('_')[1], n])
    sheet('BIEN mappings', ['trait', 'identifier', 'APD label', 'APD description', 'match', 'BIEN trait'],
          rows, [34, 15, 34, 60, 9, 45])
    rows = []
    for t in order:
        k, v = by_name[t]
        for f in ('exact_match', 'close_match', 'related_match'):
            for i in [x for x in str(v.get(f) or '').split('; ') if x]:
                r = pc.get(i, {})
                rows.append([t, k, v['label'], ws(v['description']), f.split('_')[0], i, r.get('label', ''), r.get('description', '')])
    sheet('Ontology matches', ['trait', 'identifier', 'APD label', 'APD description', 'match', 'term', 'term label',
                               'term definition'], rows, [34, 15, 34, 55, 9, 16, 30, 60])
    rows = []
    for t in order:
        k, v = by_name[t]
        rows.append([k, t, v['label'], uris[t]['basis'], '; '.join(hier.get(g, g) for g in str(v['category']).split('; ')),
                     v['created'], v['modified'], v.get('constraints', ''), v.get('exact_match', ''),
                     v.get('close_match', ''), v.get('related_match', ''), ws(v['description'])])
    sheet('New traits', ['identifier', 'trait', 'label', 'URI basis', 'category', 'created', 'modified', 'constraints',
                         'exact', 'close', 'related', 'description'], rows, [15, 34, 34, 40, 34, 11, 11, 34, 16, 16, 16, 60])
    sheet('Constraints', ['identifier', 'trait', 'constraints'], [[r[0], r[1], r[7]] for r in rows if r[7]], [15, 40, 60])
    ch = read_csv(os.path.join(args.work, 'existing_changes.csv'))
    sheet('Existing-trait edits', ['trait', 'identifier', 'field', 'APD before', 'traits.yml (applied)', 'last changed'],
          [[r['trait'], r['identifier'], r['field'], r['apd'], r['traits_yml'], r['last_changed']] for r in ch],
          [34, 15, 22, 60, 60, 12], accept=False)
    no = read_csv(os.path.join(args.work, 'conversion_notes.csv'))
    sheet('Conversion notes', ['trait', 'new trait', 'note'],
          [[r['trait'], r['new'], r['note']] for r in no if 'ambiguous' not in r['note']], [40, 8, 100], accept=False)
    del wb['Sheet']
    out = os.path.join(args.work, f'APD_import_review_{(args.today or dt.date.today().isoformat())[:7]}.xlsx')
    wb.save(out)
    print(f'{out}: ' + ', '.join(f'{x.title} {x.max_row - 1}' for x in wb.worksheets))


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument('cmd', choices=['fetch-try', 'prepare', 'apply', 'review'])
    ap.add_argument('--ab', default='../austraits.build', help='austraits.build checkout')
    ap.add_argument('--ref', default='origin/develop', help='austraits.build ref to import from')
    ap.add_argument('--since', help='austraits.build commit where traits.yml was last synced from the APD')
    ap.add_argument('--work', default='data/edit/import', help='scratch dir (gitignored)')
    ap.add_argument('--ontologies', default=os.path.expanduser(
        '~/Library/CloudStorage/OneDrive-UNSW/Documents/data_for_GitHub_repos/APD/ontologies'))
    ap.add_argument('--today', help='date for changes with no history (YYYY-MM-DD)')
    ap.add_argument('--keep', help='comma-separated APD traits to keep although traits.yml dropped them')
    args = ap.parse_args()
    if args.cmd == 'prepare' and not args.since:
        ap.error('prepare needs --since')
    {'fetch-try': cmd_fetch_try, 'prepare': cmd_prepare, 'apply': cmd_apply, 'review': cmd_review}[args.cmd](args)


if __name__ == '__main__':
    main()
