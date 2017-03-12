#!/usr/bin/env python

"""
   Kevin Angstadt
   University of Virginia
   
   Convert VASim Statevectors into Traces of RAPID line numbers
"""

import argparse, csv, json

def parse_tsv(filename):
    mapping = dict()
    with open(filename, "rb") as f:
        reader = csv.reader(f, delimiter="\t")
        for row in reader:
            mapping[row[0]] = row[1]
    
    return mapping

def process(filename, ste_to_ast, ast_to_line):
    with open(filename, "r") as f:
        lines = f.readlines()
    
    vec = dict()
    
    while(len(lines) > 0):
        offset = int(lines.pop(0).strip())
        num_stes = int(lines.pop(0).strip())
        data = list()
        for i in range(num_stes):
            ste = lines.pop(0).strip().split(",")[0]
            for ast in ste_to_ast[ste]:
                lineno = int(ast_to_line[str(ast["ast_id"])])
                data.append({
                    "ste" : ste,
                    "ast" : ast,
                    "lineno" : lineno
                })
        vec[offset] = data
    
    return vec

if __name__ == '__main__':
    parser = argparse.ArgumentParser(prog="parse_statevec.py",
                                     description="Parse the Output of VASim to generate line info from RAPID program")
    
    parser.add_argument("statevec", help="The state vector file")
    parser.add_argument("astmap", help="The .debug.ast-line file generated by the RAPID compiler")
    parser.add_argument("stemap", help="The .debug.json file generated by the RAPID compiler")
    parser.add_argument("outfile", help="where to write the resulting JSON file")
    
    args = parser.parse_args()
    
    
    ast_to_line = parse_tsv(args.astmap) 
    
    with open(args.stemap, "r") as f:
        ste_to_ast = json.load(f)
    
    line_info = process(args.statevec, ste_to_ast, ast_to_line)
    
    with open(args.outfile, "w") as of:
        json.dump(line_info, of, sort_keys=True, indent=2, separators=(',', ': '))