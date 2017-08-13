#==============================================================================
# Copyright (c) 2016-2017 Allan CORNET (Nelson)
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
import os;

def get_appveyor_repo_commit():
	return os.getenv('APPVEYOR_REPO_COMMIT');

def get_appveyor_build_number():
	return os.getenv('APPVEYOR_BUILD_NUMBER');

def get_appveyor_build_version():
	return os.getenv('APPVEYOR_BUILD_VERSION');

def use_appveyor_variables():
	return get_appveyor_repo_commit() is not None and get_appveyor_build_number() is not None and get_appveyor_build_version() is not None;

if __name__ == '__main__':

	if use_appveyor_variables() is True:
		print(get_appveyor_repo_commit())
		print(get_appveyor_build_number())
		print(get_appveyor_build_version())
