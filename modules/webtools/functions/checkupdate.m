%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = checkupdate(varargin)
  narginchk(0, 4);
  nargoutchk(0, 3);
  try
    [url, forceNoGui] = parseInputArguments(varargin);
  catch ME
    throw(ME)
  end
  haveGui = shouldDisplayGui(forceNoGui);  
  if isempty(url)
    url = getDefaultUrl();
  end
  [res, msg, html_url] = fetchLatestReleaseInfo(url, version('-semantic'));
  
  if nargout > 0
    varargout{1} = res;
    varargout{2} = msg;
    varargout{3} = html_url;
  else
    if haveGui
      messageGUI(msg, html_url);
    else
      messageCLI(msg, html_url);
    end
  end
end
%=============================================================================
function haveGui = shouldDisplayGui(forceNoGui)
  haveGui = (strcmp(getnelsonmode(), 'ADVANCED_TERMINAL') || strcmp(getnelsonmode(), 'GUI')) && ~forceNoGui;
end
%=============================================================================
function [url, forceNoGui] = parseInputArguments(inputs)
  url = '';
  forceNoGui = false;
  
  if (mod(length(inputs), 2) ~= 0)
    error(_('Wrong number of input arguments. Inputs must be name-value pairs.'));
  end
  
  for i = 1:2:length(inputs)
    name = inputs{i};
    value = inputs{i + 1};
    
    switch lower(name)
      case 'url'
        url = value;
      case 'forcenogui'
        forceNoGui = value;
      otherwise
        error(sprintf(_("Unexpected input name: '%s'."), name));
      end
    end
  end
  %=============================================================================
function messageGUI(msg, html_url)
  if ~isempty(html_url)
    msg = [msg, char(10), _('Would you like to open your web browser to access the download page?')];
    choice = questdlg (msg, _('Update Available'), _('Yes'), _('No'), _('Yes'));
    if (strcmp(choice, _('Yes')))
      openUrl(html_url);
    end
  else
    msgbox(msg, _('Check Update'), 'modal');
  end 
end
%=============================================================================
function messageCLI(msg, html_url)
  if ~isempty(html_url)
    fprintf(['\n', msg, '\n', html_url, '\n']);
  else
    fprintf(['\n', msg, '\n'])
  end
end
%=============================================================================
function url = getDefaultUrl()
  defaultsConf = [modulepath('nelson','etc'), '/defaults.conf'];
  content = fileread(defaultsConf);
  defaults = jsondecode(content);
  url = defaults.update_url;
end
%=============================================================================
function [res, msg, html_url] = fetchLatestReleaseInfo(url, currentVersion)
  res = false;
  msg = '';
  html_url = '';
  try
    if isfile(url)
      content = jsondecode(fileread(url));
    else
      content = webread(url);
    end
    isrelease = ~content.prerelease;
    if ~isrelease
      res = false;
      html_url = '';
      msg = _('Current version is up-to-date.');
      return
    end
    tag_name = content.tag_name;
    html_url = content.html_url;
    target_commitish = content.target_commitish;
    if ~strcmp(target_commitish, 'master')
      res = false;
      html_url = '';
      msg = _('Current version is up-to-date.');
      return
    end
    res = semver(version('-semantic'), tag_name);
    if res == 0
      res = false;
      msg = _('Current version is up-to-date.');
      html_url = '';
    elseif res < 0
      msg = [_('A new version is available:'), ' ', tag_name];
      res = true;
    else
      res = false;
      html_url = '';
      msg = _('Current version is up-to-date.');
    end
  catch
    msg = _('Please check url or your internet connection.');
    res = false;
  end
end
%=============================================================================
function openUrl(url)
  if ispc()
    winopen(url);
  elseif ismac()
    cmd = ['open ''', url, ''''];
    unix([cmd, '&']);
  else
    cmd = ['xdg-open ''', url, ''''];
    unix([cmd, '&']);
  end
end
%=============================================================================
