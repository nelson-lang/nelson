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
import os;
import argparse;
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
	return int(os.getenv('APPVEYOR_BUILD_NUMBER'));

def get_appveyor_build_version():
	return os.getenv('APPVEYOR_BUILD_VERSION');

def use_appveyor_variables():
	return get_appveyor_repo_commit() is not None and get_appveyor_build_number() is not None and get_appveyor_build_version() is not None;

def get_travis_build_number():
	return int(os.getenv('TRAVIS_BUILD_NUMBER'));

def get_travis_commit():
	return os.getenv('TRAVIS_COMMIT');

def use_travis():
	return os.getenv('TRAVIS') == 'true';

def edit_homepage_md(version_str):
	for (directory, _, files) in os.walk('./modules/main/help'):
		for f in files:
			path = os.path.join(directory, f)
			if os.path.exists(path):
				if path.endswith('homepage.md'):
					lines_out = [];
					with open(path) as f:
						lines_in = f.readlines()
						for line in lines_in:
							line = line.replace('\r\n', '');
							line = line.replace('\n', '');
							if line.strip().startswith('### Nelson '):
								lines_out.append('### Nelson ' + version_str);
							else:
								lines_out.append(line);
					with open(path, 'w') as f:
						for l in lines_out:
							f.write(l + '\n');

def edit_rc_file(filename, version_str):
	lines_out = [];
	with open(filename) as f:
		lines_in = f.readlines()
		for line in lines_in:
			line = line.replace('\r\n', '');
			line = line.replace('\n', '');
			if line.strip().startswith('FILEVERSION'):
				lines_out.append(' FILEVERSION ' + version_str);
			else:
				if line.strip().startswith('PRODUCTVERSION'):
					lines_out.append(' PRODUCTVERSION ' + version_str);
				else:
					if line.strip().startswith('VALUE \"ProductVersion\", \"'):
						lines_out.append('            VALUE \"ProductVersion\", \"' + version_str + '\"');
					else:
						if line.strip().startswith('VALUE \"FileVersion\", \"'):
							lines_out.append('            VALUE \"FileVersion\", \"' + version_str + '\"');
						else:
							lines_out.append(line);
	with open(filename, 'w') as f:
		for l in lines_out:
			f.write(l + '\n');

def edit_rc_files_in_modules(major, minor, maintenance, build):
	version_str = str(major) + ',' + str(minor) + ',' + str(maintenance) + ',' + str(build);
	for (directory, _, files) in os.walk('./modules'):
		for f in files:
			path = os.path.join(directory, f)
			if os.path.exists(path):
				_, file_extension = os.path.splitext(path)
				if (file_extension == '.rc'):
						edit_rc_file(path, version_str)

def edit_cmakelist(major, minor, maintenance, build):
	lines_out = [];
	filename = './CMakeLists.txt';
	with open(filename) as f:
		lines_in = f.readlines()
		for line in lines_in:
			line = line.replace('\r\n', '');
			line = line.replace('\n', '');
			if line.strip().startswith('set (Nelson_VERSION_MAJOR_DEFAULT'):
				lines_out.append('set (Nelson_VERSION_MAJOR_DEFAULT ' + str(major) +')');
			else:
				if line.strip().startswith('set (Nelson_VERSION_MINOR_DEFAULT'):
					lines_out.append('set (Nelson_VERSION_MINOR_DEFAULT ' + str(minor) +')');
				else:
					if line.strip().startswith('set (Nelson_VERSION_MAINTENANCE_DEFAULT'):
						lines_out.append('set (Nelson_VERSION_MAINTENANCE_DEFAULT ' + str(maintenance) + ')');
					else:
						if line.strip().startswith('set (Nelson_VERSION_BUILD_DEFAULT'):
							lines_out.append('set (Nelson_VERSION_BUILD_DEFAULT ' + str(build) + ')');
						else:
							lines_out.append(line);
	with open(filename, 'w') as f:
		for l in lines_out:
			f.write(l + '\n');

def edit_appveyor_yml(major, minor, maintenance):
	lines_out = [];
	filename = './appveyor.yml';
	with open(filename) as f:
		lines_in = f.readlines()
		for line in lines_in:
			line = line.replace('\r\n', '');
			line = line.replace('\n', '');
			if line.strip().startswith('version:'):
				lines_out.append('version: ' + str(major) + '.' + str(minor) + '.' + str(maintenance) + '.{build}');
			else:
				lines_out.append(line);
	with open(filename, 'w') as f:
		for l in lines_out:
			f.write(l + '\n');

def delete_nelson_version_h():
	filename = './modules/core/src/include/Nelson_VERSION.h';
	if os.path.isfile(filename) is True:
		os.remove(filename)

def edit_nelson_version_h_vc(major, minor, maintenance, build, git_hash):
	lines_out = [];
	filename = './modules/core/src/include/Nelson_VERSION.h.vc';
	with open(filename) as f:
		lines_in = f.readlines()
		for line in lines_in:
			line = line.replace('\r\n', '');
			line = line.replace('\n', '');
			if line.strip().startswith('#define NELSON_VERSION_COMMIT_HASH'):
				lines_out.append('#define NELSON_VERSION_COMMIT_HASH \"' + git_hash + '\"');
			else:
				if line.strip().startswith('#define NELSON_VERSION_MAJOR'):
					lines_out.append('#define NELSON_VERSION_MAJOR ' + str(major));
				else:
					if line.strip().startswith('#define NELSON_VERSION_MINOR'):
						lines_out.append('#define NELSON_VERSION_MINOR ' + str(minor));
					else:
						if line.strip().startswith('#define NELSON_VERSION_MAINTENANCE'):
							lines_out.append('#define NELSON_VERSION_MAINTENANCE ' + str(maintenance));
						else:
							if line.strip().startswith('#define NELSON_VERSION_BUILD'):
								lines_out.append('#define NELSON_VERSION_BUILD ' + str(build));
							else:
								lines_out.append(line);
	with open(filename, 'w') as f:
		for l in lines_out:
			f.write(l + '\n');

def edit_nelson_version_h_in(git_hash):
	lines_out = [];
	filename = './modules/core/src/include/Nelson_VERSION.h.in';
	with open(filename) as f:
		lines_in = f.readlines()
		for line in lines_in:
			line = line.replace('\r\n', '');
			line = line.replace('\n', '');
			if line.strip().startswith('#define NELSON_VERSION_COMMIT_HASH'):
				lines_out.append('#define NELSON_VERSION_COMMIT_HASH \"' + git_hash + '\"');
			else:
				lines_out.append(line);
	with open(filename, 'w') as f:
		for l in lines_out:
			f.write(l + '\n');

def get_current_version():
	version = [];
	filename = './appveyor.yml';
	version_str = None;
	with open(filename) as f:
		lines_in = f.readlines()
		for line in lines_in:
			line = line.strip();
			if line.startswith('version:'):
				version_str = line;
				break;
	if version_str is not None:
		version_str = version_str.replace('version:', '');
		version_str = version_str.replace('.{build}', '');
		split = version_str.split('.');
		if len(split) == 3:
			version = [int(split[0]), int(split[1]), int(split[2])];
	return version;

if __name__ == '__main__':
	major = None;
	minor = None;
	maintenance = None;
	build = None;
	current_version = get_current_version();
	update_from_command_line = False;

	if use_appveyor_variables() is True:
		print('USE APPVEYOR');
		print('REPO COMMIT: ' + get_appveyor_repo_commit());
		print('BUILD NUMBER: ' + str(get_appveyor_build_number()));
		print('BUILD VERSION: ' + get_appveyor_build_version());
		major = current_version[0];
		minor = current_version[1];
		maintenance = current_version[2];
		build = get_appveyor_build_number();
		git_hash = get_appveyor_repo_commit();

	else:
		if use_travis() is True:
			print('USE TRAVIS');
			print('REPO COMMIT: ' + get_travis_commit());
			print('BUILD NUMBER: ' +str(get_travis_build_number()));
			major = current_version[0];
			minor = current_version[1];
			maintenance = current_version[2];
			build = get_travis_build_number();
			git_hash = get_travis_commit();

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
			update_from_command_line = True;

	if major is None or minor is None or maintenance is None or build is None:
		sys.stderr.write('error on version definition.');
		sys.exit(1);

	version_str = str(major) + '.' + str(minor) + '.' + str(maintenance) + '.' + str(build);
	print('VERSION: ' + version_str);

	if update_from_command_line == True:
		git_hash =  get_git_revision_hash();
		if is_dirty_git():
			git_hash = get_git_revision_hash() + '_dirty';
			print('WARNING: dirty version detected.');

	print('HASH: ' + git_hash);

	edit_rc_files_in_modules(major, minor, maintenance, build);
	edit_cmakelist(major, minor, maintenance, build);
	delete_nelson_version_h();
	edit_nelson_version_h_vc(major, minor, maintenance, build, git_hash);
	edit_nelson_version_h_in(git_hash);
	if update_from_command_line == True:
		edit_appveyor_yml(major, minor, maintenance);
	edit_homepage_md(version_str);
	sys.exit(0);
