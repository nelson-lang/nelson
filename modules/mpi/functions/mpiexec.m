%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = mpiexec(varargin)
  if nargin == 0 || nargin > 2
    error(_('Wrong number of input arguments.'));
  end
  if nargout > 2
    error(_('Wrong number of output arguments.'));
  end
  
  if nargin > 1
    nb_process = int32(varargin{2});
    if ~isscalar(nb_process)
      error(_('#2 argument: integer value expected.'));
    end
  else
    nb_process = 1;
  end
  filename = varargin{1};
  if ~isfile(filename)
    error(_('A existing filename expected.'));
  end
  ext = fileparts(filename, 'extension');
  if (strcmp(ext, '.m') == false)
    error(_('An .m filename extension expected.'));
  end
  run_cmd = ['run(''', filename, ''');exit()'];
  if strcmp(getenv('OMPI_ALLOW_RUN_AS_ROOT'), '1') && strcmp(getenv('OMPI_ALLOW_RUN_AS_ROOT_CONFIRM'), '1')
    assume_as_root = ' --allow-run-as-root ';
  else
    assume_as_root = '';
  end
  cmd = ['mpiexec', assume_as_root, ' -n ', int2str(nb_process), ' ','nelson-cli', ' -q', ' -e ', '"', run_cmd, '"'];
  [res, msg] = unix(cmd);
  if nargout == 0
    disp(msg)
  end
  if nargout == 1
    varargout{1} = res;
    disp(msg)
  end
  if nargout == 2
    varargout{1} = res;
    varargout{2} = msg;
  end
  
end
%=============================================================================
