# Minor changes by Kevin Angstadt
# to conform with rapidsim output

from argparse import ArgumentParser
from collections import defaultdict
from os import popen
from re import compile

from pprint import PrettyPrinter
pp = PrettyPrinter(indent=4).pprint

def batchSim(ANML_file, stimulus_file, args='-e'):
	"""Python binding to batchSim functional ANML simulation

	Usage: If you want literal interpretation of your input symbols, 
	pass an empty string as the third argument. Otherwise, it will assume
	escaped characters, e.g. '\xb0\xff\05'"""
	ANML_file = './' + ANML_file
	stimulus_file = './' + stimulus_file
	cmd = ' '.join(['batchSim', args, ANML_file, stimulus_file])
	#print cmd
	cmd_output = popen(cmd).read()

	if '-e' in args:
		num_bytes_to_read = 4
	else:
		num_bytes_to_read = 1
	in_symbols_to_offsets = {}
	with open(stimulus_file,'r') as f:
		offset_count = 0
		in_symbol = f.read(num_bytes_to_read)
		while in_symbol:
			in_symbols_to_offsets[offset_count] = in_symbol
			in_symbol = f.read(num_bytes_to_read)
			offset_count = 1 + offset_count
		f.close()

	offsets_to_out_symbols = defaultdict(list)
	last_offset = None
	offsetElementRE = compile(r'Element id:\s(?P<element_id>.*)\sreporting at index\s(?P<element>\d+)')
	for line_num,line in enumerate(cmd_output.splitlines()):
		foo = offsetElementRE.search(line)
		if foo:
			match_element, offset = foo.groups()
			offsets_to_out_symbols[offset].append(match_element)
	return (in_symbols_to_offsets,offsets_to_out_symbols)

if __name__ == '__main__':
	parser = ArgumentParser(description=__doc__)
	parser.add_argument('-a', '--ANML_filename', required=True,
						help='ANML file filename')
	parser.add_argument('-s', '--stimulus_file', required=True,
						help='Stimulus file filename')
	args = parser.parse_args()

	in_symbols_to_offsets,offsets_to_out_symbols = batchSim(args.ANML_filename,args.stimulus_file,'')
	items_list = offsets_to_out_symbols.items()
	items_list.sort(key = lambda tup: int(tup[0]))
	for (key,value) in items_list:
		print key, '->', len(value)
	#pp(in_symbols_to_offsets)
	#pp(offsets_to_out_symbols)
