#==============================================================================
# Copyright (c) 2016-2018 Allan CORNET (Nelson)
#==============================================================================
# LICENCE_BLOCK_BEGIN
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
