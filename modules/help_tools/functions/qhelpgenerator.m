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
  if (nargin() ~= 3)
    error(_('Wrong number of input arguments.'));
  end

  src_in = varargin{1};
  dir_out = varargin{2};
  destination_file = varargin{3};
  file_generated =  [dir_out, destination_file];

  if ~isfile([src_in, '/helpproject.qhp'])
    error(_('helpproject.qhp is missing.'))
  end

  qhelpgenerator_filename = 'qhelpgenerator';
  QTDIR_BINARIES = getenv('QTDIR_BINARIES');
  if strcmp(QTDIR_BINARIES, '')
    if ismac()
      if isdir('/usr/local/opt/qt6/bin/')
        QTDIR_BINARIES = '/usr/local/opt/qt6/bin/';
      else 
        if isdir('/usr/local/opt/qt5/bin/')
          QTDIR_BINARIES = '/usr/local/opt/qt5/bin/';
        end
      end
    end
  end
  if ~strcmp(QTDIR_BINARIES, '')
    new_qhelpgenerator_filename = [QTDIR_BINARIES, '/qhelpgenerator'];
    if isfile(new_qhelpgenerator_filename)
      qhelpgenerator_filename = new_qhelpgenerator_filename;
    else
      if ismac()
        [status, msg] = unix('which qhelpgenerator');
        if status == 0
          qhelpgenerator_filename = [replace(msg, char(10), '')];
        end
      end
    end
  end
  if ~ispc() && ~ismac()
    [status, msg] = unix('which qhelpgenerator-qt6');
    if status == 0
      qhelpgenerator_filename = [replace(msg, char(10), '')];
    else
      [status, msg] = unix('which qhelpgenerator-qt5');
      if status == 0
        qhelpgenerator_filename = [replace(msg, char(10), '')];
      end
    end
  end
  if isfile(file_generated)
    [res, msg] = rmfile(file_generated);
    if res
      error(msg)
    end
  end
  cmd = [qhelpgenerator_filename, ' "', src_in, '/helpproject.qhp"', ' -o "', file_generated, '"'];
  [res, msg] = unix(cmd);
  if res
    error(msg)
  end
end
