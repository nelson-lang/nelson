#==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
#==============================================================================
# This file is part of the Nelson.
#=============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
#==============================================================================
import os
import sys
import glob

if __name__ == '__main__':
	nelson_root_path = os.path.join(os.path.dirname(os.path.abspath(__file__)), '../../')
	files_extensions = ['*.h', '*.hpp', '*.hxx', '*.c', '*.cpp', '*.cxx']
	path_prefix = ['modules/*/src/*', 'modules/*/builtin/*']
	files = []
	for p in path_prefix:
		for f in files_extensions:
			file_extension_path = os.path.join(nelson_root_path, p, f)
			files = files + glob.glob(file_extension_path, recursive=True)
	for f in files:
		filename = os.path.abspath(f)
		command = 'clang-format -i -style=file -fallback-style=none ' + filename
		print(filename)
		os.system(command)

	sys.exit(0)
