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
  end
  if ismac()
    qhelpgenerator_filename = get_help_generator_filename_macos();
    return
  end
  qhelpgenerator_filename = get_help_generator_filename_linux();
end
%=============================================================================
function qhelpgenerator_filename = get_help_generator_filename_windows()
  qhelpgenerator_filename = 'qhelpgenerator';
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
function qhelpgenerator_filename = get_help_generator_filename_macos()
  if isfile('/usr/local/share/qt/libexec/qhelpgenerator')
    qhelpgenerator_filename = '/usr/local/share/qt/libexec/qhelpgenerator';
    return
  end 
  if isfile('/usr/local/opt/qt6/bin/qhelpgenerator')
    qhelpgenerator_filename = '/usr/local/opt/qt6/bin/qhelpgenerator';
    return
  end 
  if isfile('/usr/local/opt/qt5/bin/qhelpgenerator')
    qhelpgenerator_filename = '/usr/local/opt/qt5/bin/qhelpgenerator';
    return
  end 
  QTDIR_BINARIES = get_qtdir_binaries();
  qhelpgenerator_filename = [QTDIR_BINARIES, '/qhelpgenerator'];
  if isfile(qhelpgenerator_filename)
    return
  end 
  [status, msg] = unix('which qhelpgenerator');
  if status == 0
    qhelpgenerator_filename = [replace(msg, char(10), '')];
    return
  end
  % not found :(
  qhelpgenerator_filename = 'qhelpgenerator';
end
%=============================================================================
function qhelpgenerator_filename = get_help_generator_filename_linux()
  QTDIR_BINARIES = get_qtdir_binaries();
  qhelpgenerator_filename = [QTDIR_BINARIES, '/qhelpgenerator'];
  if isfile(qhelpgenerator_filename)
    return
  end 
  [status, msg] = unix('qtpaths --query QT_HOST_LIBEXECS');
  msg = strtrim(msg);
  if status == 0 && isdir(msg)
    qhelpgenerator_filename = [msg, '/qhelpgenerator'];
    return
  end
  [status, msg] = unix('qtpaths-qt6 --query QT_HOST_LIBEXECS');
  msg = strtrim(msg);
  if status == 0 && isdir(msg)
    qhelpgenerator_filename = [msg, '/qhelpgenerator'];
    return
  end
  [status, msg] = unix('which qhelpgenerator-qt6');
  if status == 0
    qhelpgenerator_filename = [replace(msg, char(10), '')];
    return
  end
  [status, msg] = unix('which qhelpgenerator-qt5');
  if status == 0
    qhelpgenerator_filename = [replace(msg, char(10), '')];
    return
  end
  [status, msg] = unix('which qhelpgenerator');
  if status == 0
    qhelpgenerator_filename = [replace(msg, char(10), '')];
    return
  end
  qhelpgenerator_filename = 'qhelpgenerator';
end
%=============================================================================
