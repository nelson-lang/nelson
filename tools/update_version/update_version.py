# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
import os
import argparse
import subprocess
import sys
import json
import datetime



def is_dirty_git():
    status = subprocess.check_output(['git', 'status', '-uno', '--porcelain'])
    status = status.decode('utf-8')
    status = status.rstrip(os.linesep)
    return len(status) > 0


def get_git_revision_hash():
    status = subprocess.check_output(['git', 'rev-parse', 'HEAD'])
    status = status.decode('utf-8')
    status = status.rstrip(os.linesep)
    return status


def get_git_revision_short_hash():
    status = subprocess.check_output(['git', 'rev-parse', '--short', 'HEAD'])
    status = status.decode('utf-8')
    status = status.rstrip(os.linesep)
    return status


def use_github_variables():
    return get_github_repo_commit() is not None and get_github_build_number() is not None


def get_github_repo_commit():
    return os.getenv('GITHUB_SHA')


def get_github_build_number():
    return int(os.getenv('GITHUB_RUN_NUMBER'))

def edit_homepage_md(version_str):
    for (directory, _, files) in os.walk('./modules/main/help'):
        for f in files:
            path = os.path.join(directory, f)
            if os.path.exists(path):
                if path.endswith('homepage.md'):
                    lines_out = []
                    with open(path, 'r', encoding='utf-8') as f:
                        lines_in = f.readlines()
                        for line in lines_in:
                            line = line.replace('\r\n', '')
                            line = line.replace('\n', '')
                            if line.strip().startswith('### Nelson '):
                                lines_out.append('### Nelson ' + version_str)
                            else:
                                lines_out.append(line)
                    with open(path, 'w', encoding='utf-8') as f:
                        for l in lines_out:
                            f.write(l + '\n')


def edit_rc_file(filename, version_str):
    lines_out = []
    with open(filename) as f:
        lines_in = f.readlines()
        for line in lines_in:
            line = line.replace('\r\n', '')
            line = line.replace('\n', '')
            if line.strip().startswith('FILEVERSION'):
                lines_out.append(' FILEVERSION ' + version_str)
            else:
                if line.strip().startswith('PRODUCTVERSION'):
                    lines_out.append(' PRODUCTVERSION ' + version_str)
                else:
                    if line.strip().startswith('VALUE \"ProductVersion\", \"'):
                        lines_out.append(
                            '            VALUE \"ProductVersion\", \"' + version_str + '\"')
                    else:
                        if line.strip().startswith('VALUE \"FileVersion\", \"'):
                            lines_out.append(
                                '            VALUE \"FileVersion\", \"' + version_str + '\"')
                        else:
                            lines_out.append(line)
    with open(filename, 'w') as f:
        for l in lines_out:
            f.write(l + '\n')


def edit_rc_files_in_modules(major, minor, maintenance, build):
    version_str = str(major) + ',' + str(minor) + ',' + \
        str(maintenance) + ',' + str(build)
    for (directory, _, files) in os.walk('./modules'):
        for f in files:
            path = os.path.join(directory, f)
            if os.path.exists(path):
                _, file_extension = os.path.splitext(path)
                if (file_extension == '.rc'):
                    edit_rc_file(path, version_str)


def edit_cmakelist(major, minor, maintenance, build):
    lines_out = []
    filename = './CMakeLists.txt'
    with open(filename) as f:
        lines_in = f.readlines()
        for line in lines_in:
            line = line.replace('\r\n', '')
            line = line.replace('\n', '')
            if line.strip().startswith('set(Nelson_VERSION_MAJOR_DEFAULT'):
                lines_out.append(
                    'set(Nelson_VERSION_MAJOR_DEFAULT ' + str(major) + ')')
            else:
                if line.strip().startswith('set(Nelson_VERSION_MINOR_DEFAULT'):
                    lines_out.append(
                        'set(Nelson_VERSION_MINOR_DEFAULT ' + str(minor) + ')')
                else:
                    if line.strip().startswith('set(Nelson_VERSION_MAINTENANCE_DEFAULT'):
                        lines_out.append(
                            'set(Nelson_VERSION_MAINTENANCE_DEFAULT ' + str(maintenance) + ')')
                    else:
                        if line.strip().startswith('set(Nelson_VERSION_BUILD_DEFAULT'):
                            lines_out.append(
                                'set(Nelson_VERSION_BUILD_DEFAULT ' + str(build) + ')')
                        else:
                            lines_out.append(line)
    with open(filename, 'w') as f:
        for l in lines_out:
            f.write(l + '\n')


def delete_nelson_version_h():
    filename = './modules/commons/src/include/Nelson_VERSION.h'
    if os.path.isfile(filename) is True:
        os.remove(filename)


def edit_nelson_version_h_vc(major, minor, maintenance, build, git_hash):
    lines_out = []
    filename = './modules/commons/src/include/Nelson_VERSION.h.vc'
    with open(filename) as f:
        lines_in = f.readlines()
        for line in lines_in:
            line = line.replace('\r\n', '')
            line = line.replace('\n', '')
            if line.strip().startswith('#define NELSON_VERSION_COMMIT_HASH'):
                lines_out.append(
                    '#define NELSON_VERSION_COMMIT_HASH \"' + git_hash + '\"')
            else:
                if line.strip().startswith('#define NELSON_VERSION_MAJOR'):
                    lines_out.append(
                        '#define NELSON_VERSION_MAJOR ' + str(major))
                else:
                    if line.strip().startswith('#define NELSON_VERSION_MINOR'):
                        lines_out.append(
                            '#define NELSON_VERSION_MINOR ' + str(minor))
                    else:
                        if line.strip().startswith('#define NELSON_VERSION_MAINTENANCE'):
                            lines_out.append(
                                '#define NELSON_VERSION_MAINTENANCE ' + str(maintenance))
                        else:
                            if line.strip().startswith('#define NELSON_VERSION_BUILD'):
                                lines_out.append(
                                    '#define NELSON_VERSION_BUILD ' + str(build))
                            else:
                                lines_out.append(line)
    with open(filename, 'w') as f:
        for l in lines_out:
            f.write(l + '\n')


def edit_nelson_version_h_in(git_hash):
    lines_out = []
    filename = './modules/commons/src/include/Nelson_VERSION.h.in'
    with open(filename) as f:
        lines_in = f.readlines()
        for line in lines_in:
            line = line.replace('\r\n', '')
            line = line.replace('\n', '')
            if line.strip().startswith('#define NELSON_VERSION_COMMIT_HASH'):
                lines_out.append(
                    '#define NELSON_VERSION_COMMIT_HASH \"' + git_hash + '\"')
            else:
                lines_out.append(line)
    with open(filename, 'w') as f:
        for l in lines_out:
            f.write(l + '\n')


def get_current_version():
    version = []
    filename = './package.json'
    version_str = None
    with open(filename) as json_file:
        data = json.load(json_file)
        version_str = data['version']
    if version_str is not None:
        split = version_str.split('.')
        if len(split) == 3:
            version = [int(split[0]), int(split[1]), int(split[2])]
    return version


def edit_package_json(major, minor, maintenance):
    filename = './package.json'
    with open(filename) as json_file:
        data = json.load(json_file)
    data['version'] = str(major) + '.' + str(minor) + '.' + str(maintenance)
    with open(filename, 'w') as outfile:
        json.dump(data, outfile, indent=4)


def edit_desktop_file(filename, version_str):
    lines_out = []
    with open(filename, encoding='utf-8') as f:
        lines_in = f.readlines()
        for line in lines_in:
            line = line.replace('\r\n', '')
            line = line.replace('\n', '')
            if line.strip().startswith('X-Nelson-Version='):
                lines_out.append('X-Nelson-Version=' + version_str)
            else:
                lines_out.append(line)
    with open(filename, 'w', encoding='utf-8') as f:
        for l in lines_out:
            f.write(l + '\n')


def edit_desktop_files(major, minor, maintenance):
    version_str = str(major) + '.' + str(minor) + '.' + str(maintenance)
    for (directory, _, files) in os.walk('./desktop'):
        for f in files:
            path = os.path.join(directory, f)
            if os.path.exists(path) and path.endswith('.desktop'):
                edit_desktop_file(path, version_str)


def edit_appdata_file(filename, version_str, date_str):
    import re
    lines_out = []
    with open(filename, encoding='utf-8') as f:
        lines_in = f.readlines()
        for line in lines_in:
            s = line.replace('\r\n', '').replace('\n', '')
            # update release element attributes if present on the line
            if '<release ' in s and 'version=' in s:
                s = re.sub(r'version="[^"]*"', 'version="' + version_str + '"', s)
                if 'date=' in s:
                    s = re.sub(r'date="[^"]*"', 'date="' + date_str + '"', s)
                else:
                    # add date attribute if missing
                    s = s.replace('<release ', '<release date="' + date_str + '" ', 1)
                lines_out.append(s)
            else:
                lines_out.append(s)
    with open(filename, 'w', encoding='utf-8') as f:
        for l in lines_out:
            f.write(l + '\n')


def edit_appdata_files(major, minor, maintenance):
    version_str = str(major) + '.' + str(minor) + '.' + str(maintenance)
    date_str = datetime.date.today().isoformat()
    for (directory, _, files) in os.walk('./desktop'):
        for f in files:
            path = os.path.join(directory, f)
            if os.path.exists(path) and path.endswith('.appdata.xml'):
                edit_appdata_file(path, version_str, date_str)


if __name__ == '__main__':
    major = None
    minor = None
    maintenance = None
    build = None
    current_version = get_current_version()
    update_from_command_line = False

    if use_github_variables() is True:
        print('USE GITHUB')
        print('REPO COMMIT: ' + get_github_repo_commit())
        print('BUILD NUMBER: ' + str(get_github_build_number()))
        major = current_version[0]
        minor = current_version[1]
        maintenance = current_version[2]
        build = get_github_build_number()
        git_hash = get_github_repo_commit()
    else:
        print('USE COMMAND LINE')
        parser = argparse.ArgumentParser(
            description='Update Nelson version.')
        parser.add_argument("major", type=int,
                            help='major version index', default=None)
        parser.add_argument("minor", type=int,
                            help='minor version index', default=None)
        parser.add_argument(
            "maintenance", help='maintenance version index', type=int, default=None)
        parser.add_argument(
            "build", help='build version index', type=int, default=None)
        args = parser.parse_args()
        major = args.major
        minor = args.minor
        maintenance = args.maintenance
        build = args.build
        update_from_command_line = True

    if major is None or minor is None or maintenance is None or build is None:
        sys.stderr.write('error on version definition.')
        sys.exit(1)

    version_str = str(major) + '.' + str(minor) + '.' + \
        str(maintenance) + '.' + str(build)
    print('VERSION: ' + version_str)

    if update_from_command_line == True:
        git_hash = get_git_revision_hash()
        if is_dirty_git():
            git_hash = get_git_revision_hash() + '_dirty'
            print('WARNING: dirty version detected.')

    print('HASH: ' + git_hash)

    edit_rc_files_in_modules(major, minor, maintenance, build)
    edit_cmakelist(major, minor, maintenance, build)
    delete_nelson_version_h()
    edit_nelson_version_h_vc(major, minor, maintenance, build, git_hash)
    edit_nelson_version_h_in(git_hash)
    if update_from_command_line == True:
        edit_package_json(major, minor, maintenance)
    edit_homepage_md(version_str)
    edit_desktop_files(major, minor, maintenance)
    edit_appdata_files(major, minor, maintenance)

    sys.exit(0)
