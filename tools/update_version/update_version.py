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
import argparse;
from os import walk;
import subprocess;
import sys;

def is_dirty_git():
	status = subprocess.check_output('git status -uno --porcelain');
	status = status.decode('utf-8');
	status =status.rstrip(os.linesep);
	return len(status) > 0;

def get_git_revision_hash():
	status = subprocess.check_output(['git', 'rev-parse', 'HEAD']);
	status = status.decode('utf-8');
	status =status.rstrip(os.linesep);
	return status;

def get_git_revision_short_hash():
	status = subprocess.check_output(['git', 'rev-parse', '--short', 'HEAD']);
	status = status.decode('utf-8');
	status =status.rstrip(os.linesep);
	return status;

def get_appveyor_repo_commit():
	return os.getenv('APPVEYOR_REPO_COMMIT');

def get_appveyor_build_number():
	return os.getenv('APPVEYOR_BUILD_NUMBER');

def get_appveyor_build_version():
	return os.getenv('APPVEYOR_BUILD_VERSION');

def use_appveyor_variables():
	return get_appveyor_repo_commit() is not None and get_appveyor_build_number() is not None and get_appveyor_build_version() is not None;

def get_travis_build_number():
	return os.getenv('TRAVIS_BUILD_NUMBER');

def get_travis_commit():
	return os.getenv('TRAVIS_COMMIT');

def use_travis():
	return os.getenv('TRAVIS') == 'true';

def edit_rc_file(filename, version_str):
	lines_out = [];
	with open(filename) as f:
		lines_in = f.readlines()
		for line in lines_in:
			if line.strip().startswith('FILEVERSION'):
				lines_out.append(' FILEVERSION ' + version_str +os.linesep);
			else:
				if line.strip().startswith('PRODUCTVERSION'):
					lines_out.append(' PRODUCTVERSION ' + version_str +os.linesep);
				else:
					if line.strip().startswith('VALUE \"ProductVersion\", \"'):
						lines_out.append('            VALUE \"ProductVersion\", \"' + version_str + '\"' +os.linesep);
					else:
						if line.strip().startswith('VALUE \"FileVersion\", \"'):
							lines_out.append('            VALUE \"FileVersion\", \"' + version_str + '\"' +os.linesep);
						else:
							lines_out.append(line);

	with open(filename, 'w') as f:
		f.writelines(lines_out);

def edit_rc_files_in_modules(major, minor, maintenance, build):
	version_str = str(major) + ',' + str(minor) + ',' + str(maintenance) + ',' + str(build);
	for (dir, _, files) in os.walk('./modules'):
		for f in files:
			path = os.path.join(dir, f)
			if os.path.exists(path):
				file_name, file_extension = os.path.splitext(path)
				if (file_extension == '.rc'):
						edit_rc_file(path, version_str)

def edit_cmakelist(major, minor, maintenance, build):
	lines_out = [];
	filename = './CMakeLists.txt';
	with open(filename) as f:
		lines_in = f.readlines()
		for line in lines_in:
			if line.strip().startswith('set (Nelson_VERSION_MAJOR_DEFAULT'):
				lines_out.append('set (Nelson_VERSION_MAJOR_DEFAULT ' + major +')' + os.linesep);
			else:
				if line.strip().startswith('set (Nelson_VERSION_MINOR_DEFAULT'):
					lines_out.append('set (Nelson_VERSION_MINOR_DEFAULT ' + minor +')' + os.linesep);
				else:
					if line.strip().startswith('set (Nelson_VERSION_MAINTENANCE_DEFAULT'):
						lines_out.append('set (Nelson_VERSION_MAINTENANCE_DEFAULT ' + maintenance + ')' + os.linesep);
					else:
						if line.strip().startswith('set ((Nelson_VERSION_BUILD_DEFAULT'):
							lines_out.append('set ((Nelson_VERSION_BUILD_DEFAULT ' + build + ')' + os.linesep);
						else:
							lines_out.append(line);

	with open(filename, 'w') as f:
		f.writelines(lines_out);

def edit_nelson_version_h_vc(major, minor, maintenance, build, git_hash):
	pass

def edit_nelson_version_h_in(git_hash):
	pass

	
	
if __name__ == '__main__':
	
	major = None;
	minor = None;
	maintenance = None;
	build = None;


	if use_appveyor_variables() is True:
		print('USE APPVEYOR');
		print('REPO COMMIT: ' + get_appveyor_repo_commit());
		print('BUILD NUMBER: ' + get_appveyor_build_number());
		print('BUILD VERSION: ' + get_appveyor_build_version());
	else:
		if use_travis() is True:
			print('USE TRAVIS');
			print('REPO COMMIT: ' + get_travis_commit());
			print('BUILD NUMBER: ' +get_travis_build_number());
		else:
			print('USE COMMAND LINE');
			parser = argparse.ArgumentParser(description='Update Nelson version.');
			parser.add_argument("major", type=int, help = 'major version index', default = None)
			parser.add_argument("minor", type=int, help = 'minor version index', default = None)
			parser.add_argument("maintenance", help='maintenance version index', type=int, default = None)
			parser.add_argument("build", help='build version index', type=int, default = None)
			args = parser.parse_args();
			major = args.major;
			minor = args.minor;
			maintenance = args.maintenance;
			build = args.build;
			version_str = str(major) + '.' + str(minor) + '.' + str(maintenance) + '.' + str(build);
			print('VERSION:' +version_str);

	if major is None or minor is None or maintenance is None or build is None:
		sys.stderr.write('error on version definition.');
		sys.exit(1);

	git_hash =  get_git_revision_hash();
	if is_dirty_git():
		git_hash = get_git_revision_hash() + '_dirty';
		print('WARNING: dirty version detected.');

#	edit_rc_files_in_modules(major, minor, maintenance, build);
#	edit_cmakelist(major, minor, maintenance, build);
#	edit_nelson_version_h_vc(major, minor, maintenance, build, git_hash);
#	edit_nelson_version_h_in(git_hash);
	sys.exit(0);
