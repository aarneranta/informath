from itertools import groupby
import pgf

# to be run and then filtered manually: redirect to terms.tmp
# gf -make DerivedMathTermsEng.gf DerivedMathTermsFre.gf DerivedMathTermsGer.gf DerivedMathTermsSwe.gf

grammar = pgf.readPGF('DerivedMathTerms.pgf')

eng = grammar.languages['DerivedMathTermsEng']
fre = grammar.languages['DerivedMathTermsFre']
ger = grammar.languages['DerivedMathTermsGer']
swe = grammar.languages['DerivedMathTermsSwe']

langs = [eng, fre, ger, swe]


def iflin(s):
    return 'None' if s.startswith('[') else s

funs = []
for fun in grammar.functions:
    exp = pgf.Expr(fun, [])
    funs.append(([iflin(lang.linearize(exp)) for lang in langs], fun))

funs.sort(key = lambda e: e[0])

def clean_word(w):
    return w != 'None' and len(w.split()) == 1

gfuns = [list(g) for k, g in  groupby(funs, key=lambda e: e[0])]
gfuns = [(g[0][0], [f[1] for f in g]) for g in gfuns]
clean_gfuns = [f for f in gfuns if clean_word(f[0][0]) and clean_word(f[0][1])]

# to create data for manual inspection: redirect to terms.tmp and remove unwanted lines
if __name__ == '__main1__':
    for fun in clean_gfuns:
        print(fun)

# to test data integrity
if __name__ == '__main2__':
    with open('terms.tmp') as file:
        for line in file:
            print(type(eval(line)))

def mk_fun(s):
    s = '_'.join(s.split())  # spaces replaced by underscores
    
    if (s[0].isalpha() and
        all(ord(c)<256 and (c.isdigit() or c.isalpha() or c in "_'")
            for c in s)):  # test if legal GF identifier
        return s
    else:
        return "'" + s.replace("'", "\\'") + "'"  # if not, single quotes make it legal
            
def gf_abs_rule(t):
    cat = t[1][0][-2:]
    ids = [t.split('_')[-3] for t in t[1]]
    fun = mk_fun(' '.join([w for w in t[0][:-1] if w != 'None'] + [cat]))
    return f'fun {fun} : {cat} ; -- {", ".join(ids)}'


# to generate GF
if __name__ == '__main__':
    with open('filtered-terms.tmp') as file:
        for line in file:
            print(gf_abs_rule(eval(line)))



        

