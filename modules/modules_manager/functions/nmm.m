%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = nmm(varargin)
  if nargin == 0
    error(_('Wrong number of input arguments.'));
  end
  if ~ismodule('webtools')
    error(_('nmm required webtools module.'));
  end
  if ~ismodule('file_archiver')
    error(_('nmm required file_archiver module.'));
  end
  if ~ismodule('json')
    error(_('nmm required json module.'));
  end
  
  varargout = {};
  firstParameter = varargin{1};
  if ~isSupportedFirstParameter(firstParameter)
    error('#1 value or type not managed.');
  end
  try
    switch firstParameter
      case 'list'
        if nargin ~= 1
          error(_('Wrong number of input arguments.'));
        end
        varargout{1} = nmm_list();
      case 'load'
        if nargout ~= 0
          error(_('Wrong number of output arguments.'));
        end
        nmm_load(varargin{2:end});
      case 'autoload'
        varargout{1} = nmm_autoload(varargin{2:end});
      case 'uninstall'
        nmm_uninstall(varargin{2:end});
      case 'install'
        nmm_install(varargin{2:end});
      case 'package'
        varargout{1} = nmm_package(varargin{2:end});
      otherwise
        error('#1 value or type not managed.')
      end
    catch
      e = lasterror();
      error(e);
    end
  end
  %=============================================================================
function r = isSupportedFirstParameter(param)
  if ischar(param)
    supported = ["list", "load", "autoload", "install", "uninstall", "package"];
    r = any(contains(supported, param));
  else
    r = false;
  end
end
%=============================================================================
