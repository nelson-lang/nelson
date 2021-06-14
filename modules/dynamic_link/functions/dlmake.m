%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later version.
%
% Alternatively, you can redistribute it and/or
% modify it under the terms of the GNU General Public License as
% published by the Free Software Foundation; either version 2 of
% the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with this program. If not, see <http://www.gnu.org/licenses/>.
% LICENCE_BLOCK_END
%=============================================================================
function [status, message] = dlmake(destinationdir)
  status = false;
  message = '';
  if ~isdir(destinationdir)
    error(_('Invalid directory.'));
  end
  if ~isfile([destinationdir, '/CMakeLists.txt'])
    error(_('CMakeLists.txt is missing.'));
  end
  [status, compiler] = havecompiler();
  if ~status
    message = _('No compiler configured.');
    return
  end
  currentdir = pwd();
  cd(destinationdir);
  if ispc()
    if strcmp(compiler, 'msvc')
      options = '"NMake Makefiles"';
      make_name = 'nmake';
    end
    if strcmp(compiler, 'mingw')
      options = '"MinGW Makefiles"';
      status = unix('mingw32-make --version');
      if status == 0
        make_name = 'mingw32-make';
      else
        make_name = 'make';
      end
    end
  else
    options = '''Unix Makefiles''';
    make_name = 'make';
  end
  [status, msg] = cmake ('-G', options);
  message = cleanup(msg);
  if ~status
    cd(currentdir);
    return
  end
  [status, msg] = unix(make_name);
  status = (status == 0);
  message = [message, cleanup(msg)];
  cd(currentdir);
end
%=============================================================================
function msgout = cleanup(msgin)
  msgout = replace(msgin, ['Failed to create ConsoleBuf!', char(10)], '');
  msgout = replace(msgout, ['setActiveInputCodepage failed!', char(10)], '');
end
%=============================================================================
