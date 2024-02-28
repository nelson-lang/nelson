%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function qhelpgenerator(varargin)
  % internal function not documented
  narginchk(3, 3);
  
  src_in = varargin{1};
  dir_out = varargin{2};
  destination_file = varargin{3};
  file_generated =  [dir_out, destination_file];
  
  if ~isfile([src_in, '/helpproject.qhp'])
    error(_('helpproject.qhp is missing.'))
  end
  
  if isfile(file_generated)
    [res, msg] = rmfile(file_generated);
    if res
      error(msg)
    end
  end
  
  qhelpgenerator_filename = get_help_generator_filename();
  
  cmd = [qhelpgenerator_filename, ' "', src_in, '/helpproject.qhp"', ' -o "', file_generated, '"'];
  [res, msg] = unix(cmd);
  if res
    error(msg)
  end
end
%=============================================================================
function qhelpgenerator_filename = get_help_generator_filename()
  if ispc()
    qhelpgenerator_filename = get_help_generator_filename_windows();
    return
  else
    qhelpgenerator_filename = get_help_generator_filename_macos_linux();
  end
end
%=============================================================================
function qhelpgenerator_filename = get_help_generator_filename_windows()
  qhelpgenerator_filename = ['"', modulepath('nelson', 'bin'), '/', 'qhelpgenerator', '"'];
end
%=============================================================================
function QTDIR_BINARIES = get_qtdir_binaries()
  QTDIR_BINARIES = getenv('QTDIR_BINARIES');
  if isempty(QTDIR_BINARIES)
    QTDIR = getenv('QTDIR');
    if ~isempty(QTDIR)
      QTDIR_BINARIES = [QTDIR, '/bin']
    end
  end
end
%=============================================================================
function qhelpgenerator_filename = get_help_generator_filename_qtpaths()
  possible_qhelpgeneration_unix_cmd = {'qtpaths --query QT_HOST_LIBEXECS', ...
  'qtpaths-qt6 --query QT_HOST_LIBEXECS'};
  for cmd = possible_qhelpgeneration_unix_cmd
    [status, msg] = unix(cmd{1});
    if status == 0
      qhelpgenerator_filename = [replace(strtrim(msg), char(10), ''), '/qhelpgenerator'];
      if isfile(qhelpgenerator_filename)
        return
      end
    end
  end
  qhelpgenerator_filename = '';
end
%=============================================================================
function qhelpgenerator_filename = get_help_generator_filename_qtdir_binaries()
  QTDIR_BINARIES = get_qtdir_binaries();
  qhelpgenerator_filename = [QTDIR_BINARIES, '/qhelpgenerator'];
  if isfile(qhelpgenerator_filename)
    return
  end 
  qhelpgenerator_filename = '';
end
%=============================================================================
function qhelpgenerator_filename = get_help_generator_filename_which()
  possible_qhelpgeneration_unix_cmd = {'which qhelpgenerator-qt6', ...
  'which qhelpgenerator-qt5', ...
  'which qhelpgenerator'};
  for cmd = possible_qhelpgeneration_unix_cmd
    [status, msg] = unix(cmd{1});
    if status == 0
      qhelpgenerator_filename = replace(strtrim(msg), char(10), '');
      if isfile(qhelpgenerator_filename)
        return
      end
    end
  end
  qhelpgenerator_filename = '';
end
%=============================================================================
function qhelpgenerator_filename = get_help_generator_filename_by_hardcoded_path()
  
  possible_qhelpgeneration_filenames = {[getenv('HOMEBREW_PREFIX'), '/share/qt/libexec/qhelpgenerator'], ...
  '/usr/local/share/qt/libexec/qhelpgenerator', ...
  '/usr/local/opt/qt6/bin/qhelpgenerator', ...
  '/usr/local/opt/qt5/bin/qhelpgenerator', ...
  '/usr/lib/qt6/bin/qhelpgenerator'};
  for filename = possible_qhelpgeneration_filenames
    if isfile(filename{1})
      qhelpgenerator_filename = filename{1};
      return
    end
  end 
  qhelpgenerator_filename = '';
end
%=============================================================================
function qhelpgenerator_filename = get_help_generator_filename_macos_linux()
  qhelpgenerator_filename = get_help_generator_filename_by_hardcoded_path();
  if ~isempty(qhelpgenerator_filename)
    return
  end
  qhelpgenerator_filename = get_help_generator_filename_qtdir_binaries();
  if ~isempty(qhelpgenerator_filename)
    return
  end
  qhelpgenerator_filename = get_help_generator_filename_qtpaths();
  if ~isempty(qhelpgenerator_filename)
    return
  end
  qhelpgenerator_filename = get_help_generator_filename_which();
  if ~isempty(qhelpgenerator_filename)
    return
  end
  % not found :(
  qhelpgenerator_filename = 'qhelpgenerator';
end
%=============================================================================
