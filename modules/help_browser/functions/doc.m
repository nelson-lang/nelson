%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function doc(varargin)
  mainUrl = docroot();
  txt = '';
  for i = 1:nargin
    name = varargin{1};
    mustBeTextScalar(name, i);
    if i == 1
      txt = name;
    else
      txt = [txt, ' ', name];
    end
  end
  if startsWith(mainUrl, 'http')
    docWeb(mainUrl, txt);
  else
    docEmbedded(txt);
  end
end
%=============================================================================
function docWeb(mainUrl, txt)
  ver_number = version('-number');
  version_string = sprintf('v%d.%d.%d', ver_number(1), ver_number(2), ver_number(3));
  language = 'en_US';
  if isempty(txt)
    url = [mainUrl, '/', language, '/', version_string, '/', 'index.html'];
    openBrowser(url);
    return
  end
  if ismacro(txt) || isbuiltin(txt)
    p = which(txt,'-module');
    if ~isempty(p)
      if ~ismodule(p{1}, 'isprotected')
        docEmbedded(txt);
      else
        url = [mainUrl, '/', language, '/', version_string, '/', p{1}, '/', txt, '.html'];
        openBrowser(url);
      end
      return
    end
  end
  if ismodule(txt)
    if ~ismodule(txt, 'isprotected')
      docEmbedded(txt);
    else
      url = [mainUrl, '/', language, '/', version_string, '/', txt, '/', 'index.html'];
      openBrowser(url);
    end
    return
  end
  url = [mainUrl, '/', language, '/', version_string, '/', 'index.html?search=', urlencode(txt)];
  openBrowser(url);
end
%=============================================================================
function docEmbedded(txt)
  if ~indexhelp()
    helpbrowser();
  end
  if isempty(txt)
    helpbrowser('-show');
    return
  end
  if ismacro(txt) || isbuiltin(txt)
    helpbrowser('-name', txt);
    helpbrowser('-show');
    return
  end
  if ismodule(txt)
    helpbrowser('-module', txt);
    helpbrowser('-show');
    return
  end
  p = which(txt,'-module');
  if isempty(p)
    helpbrowser('-name', txt);
    helpbrowser('-show');
    return
  end
  if length(p) == 1
    id = [p{1}, '::', char(txt)];
    helpbrowser('-identifier', id);
  else
    helpbrowser('-name', txt);
  end
  helpbrowser('-show');
end
%=============================================================================
function openBrowser(url)
  if ispc()
    winopen(url);
    return
  end
  if ismac()
    unix(['open ', url]);
  else
    unix(['xdg-open ', url]);
  end
end
%==============================================================================
