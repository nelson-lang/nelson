%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = deployhelp(varargin)
    % deployhelp('install')
    % deployhelp('add', module_name, module_help_dir)
    % deployhelp('remove', module_name)
    % [status, message] = deployhelp('uninstall')
    % status = deployhelp('status')
    % [status, message] = deployhelp('refresh')
    if nargin() == 0
        action = 'install';
    else
        action = varargin{1};
    end
    status = false;
    message = '';
    switch lower(action)
        case 'add'
            if ~deployhelp_status()
                install_help(true);
                docroot('.');
            end
            [status, message] = deployhelp_add(varargin{2}, varargin{3});
            if status
                [status, message] = refresh_help(varargin{2});
            end
        case 'remove'
            [status, message] = deployhelp_remove(varargin{2});
            if status
                [status, message] = refresh_help();
            end
        case 'install'
            narginchk(0, 2);
            if nargin == 2
                verbose = varargin{2};
                mustBeLogicalScalar(verbose, 2);
            else
                verbose = true;
            end
            docroot('.');
            install_help(verbose);
        case 'uninstall'
            [status, message] = uninstall_help();
        case 'status'
            narginchk(1, 1);
            nargoutchk(0, 1);
            status = deployhelp_status();
            message = '';
        case 'refresh'
            [status, message] = refresh_help();
        otherwise
            status = false;
            message = sprintf(_('Unknown action: %s'), action);
    end

    if (nargout == 0 && ~status)
      error(message);
    end
    if nargout >= 1
      varargout{1} = status;
    end
    if nargout >= 2
      varargout{2} = message;
    end
 end
%=============================================================================
function status = deployhelp_status()
  ver_number = version('-number');
  version_string = sprintf('%d.%d.%d', ver_number(1), ver_number(2), ver_number(3));
  help_dir = [userdir(), '/Nelson/', version_string, '/help/'];
  status = isdir(help_dir);
end
%=============================================================================
function [status, message] = deployhelp_add(module_name, module_help_dir)
  status = false;
  message = '';
  
  ver_number = version('-number');
  version_string = sprintf('%d.%d.%d', ver_number(1), ver_number(2), ver_number(3));
  help_dir = [userdir(), '/Nelson/', version_string, '/help/'];
  if ~isdir(help_dir)
    status = false;
    message = _('Help directory does not exist.');
    return;
  end
  for lang = getavailablelanguages()'
    lang_dir = [help_dir, '/', lang{1}];

    help_archive_name = [module_help_dir, '/help/', nelsonappid(), '.modules.', module_name, '.help.', lang{1}, '.nhz'];
    if ~isfile(help_archive_name)
      continue;
    end
    unzip(help_archive_name, lang_dir);
  end
  status = true;
  message = '';
end
%=============================================================================
function [status, message] = deployhelp_remove(module_name)
  status = false;
  message = '';
  ver_number = version('-number');
  version_string = sprintf('%d.%d.%d', ver_number(1), ver_number(2), ver_number(3));
  help_dir = [userdir(), '/Nelson/', version_string, '/help/'];
  if ~isdir(help_dir)
    status = false;
    message = _('Help directory does not exist.');
    return;
  end
  for lang = getavailablelanguages()'
    lang_dir = [help_dir, '/', lang{1}];
    if isdir(lang_dir)
      module_help_dir = [lang_dir, '/', module_name];
      if isdir(module_help_dir)
        [status, message] = rmdir(module_help_dir, 's');
        if ~status
          return;
        end
      end
    end
  end
end
%=============================================================================
