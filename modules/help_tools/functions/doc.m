%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function doc(varargin)
  m = getnelsonmode();
  if contains(getnelsonmode(), {'BASIC_ENGINE', 'BASIC_TERMINAL'})
    warning(_('Documentation is not available in this mode. See "help" command for more information.'));
    return
  end
  isSIO = contains(m, {'BASIC_SIO_CLIENT', 'ADVANCED_SIO_CLIENT'});
  mainUrl = docroot();
  if isSIO
    isWebUrl = startsWith(mainUrl, 'http://') || startsWith(mainUrl, 'https://');
    if ~isWebUrl
      mainUrl = getDefaultUrl();
    end
  end
  islocalhelp = strcmp(mainUrl, '.');
  islocalhelp = islocalhelp && ~isSIO;
  ver_number = version('-number');
  version_string = sprintf('%d.%d.%d', ver_number(1), ver_number(2), ver_number(3));

  if islocalhelp
    version_string = sprintf('%d.%d.%d', ver_number(1), ver_number(2), ver_number(3));
    mainUrl = [userdir(), 'Nelson/', version_string, '/help'];
    if ~isdir(mainUrl)
      warning(_('Local help not installed. Open web help instead.'));
      islocalhelp = false;
      mainUrl = getDefaultUrl();
    end
  else
    version_string = sprintf('v%d.%d.%d', ver_number(1), ver_number(2), ver_number(3));
  end
  txt = joinText(varargin);
  language = getlanguage();
  [stSearchResults, baseUrl] = getSearchBase(mainUrl, language, version_string, islocalhelp);
  url = buildUrl(baseUrl, txt, islocalhelp, stSearchResults);
  openBrowser(url, islocalhelp);
end
%=============================================================================
function [stSearchResults, baseUrl] = getSearchBase(mainUrl, language, version_string, islocalhelp)
  if islocalhelp
    baseUrl = [mainUrl, '/', language];
    jsonFile = [baseUrl, '/', 'index.json'];
    if ~isfile(jsonFile)
      jsonFile = [mainUrl, '/', getdefaultlanguage(), '/', 'index.json'];
      baseUrl = [mainUrl, '/', getdefaultlanguage()];
    end
    if ~isfile(jsonFile)
      error(_('Local help not installed.'));
    end
  else
    baseUrl = [mainUrl, '/', language, '/', version_string];
    jsonFile = getCacheFileHelpLang(baseUrl, language);
  end
  try
    stSearchResults = jsondecode(jsonFile, '-file');
  catch
    stSearchResults = struct([]);
  end
end
%=============================================================================
function jsonFile = getCacheFileHelpLang(baseUrl, lang)
  jsonFile = '';
  ver_number = version('-number');
  version_string = sprintf('%d.%d.%d', ver_number(1), ver_number(2), ver_number(3));
  cachedir = [userdir(), 'Nelson/', version_string, '/cache/help/', lang, '/'];
  if ~isfolder(cachedir)
    mkdir(cachedir);
  end
  cacheFile = [cachedir, 'index.json'];
  if ~isfile(cacheFile)
    jsonSearchUrl = [baseUrl, '/', 'index.json'];
    try
      jsonFile = websave(cacheFile, jsonSearchUrl, weboptions('ContentType','json'));
    catch
      jsonFile = '';
    end
  else
    jsonFile = cacheFile;
  end
end
%=============================================================================
function txt = joinText(args)
  txt = '';
  for i = 1:length(args)
    name = args{i};
    mustBeTextScalar(name, i);
    if i == 1
      txt = name;
    else
      txt = [txt, ' ', name];
    end
  end
end
%=============================================================================
function url = buildUrl(baseUrl, txt, islocalhelp, stSearchResults)
  if isempty(txt)
    url = [baseUrl, '/', 'index.html'];
    return;
  end
  urlfound = '';
  if ismacro(txt) || isbuiltin(txt)
    urlfound = searchMacroOrBuiltin(stSearchResults, txt);
  end
  if isempty(urlfound) && ismodule(txt)
    urlfound = searchModule(stSearchResults, txt);
  end
  if isempty(urlfound)
    urlfound = searchAny(stSearchResults, txt);
  end
  if ~isempty(urlfound)
    url = [baseUrl, '/', 'index.html?open=', urlfound];
  else
    url = [baseUrl, '/', 'index.html?search=', urlencode(txt)];
  end
end
%=============================================================================
function urlfound = searchMacroOrBuiltin(stSearchResults, query)
  urlfound = '';
  for i = 1:length(stSearchResults)
    entry = stSearchResults(i);
    if strcmp(entry.title, query)
      urlfound = entry.url;
      return;
    end
  end
end
%=============================================================================
function urlfound = searchModule(stSearchResults, query)
  urlfound = '';
  for i = 1:length(stSearchResults)
    entry = stSearchResults(i);
    if strcmp(entry.path, ['sections/', query])
      urlfound = ['./', query, '/index.html'];
      return;
    end
  end
end
%=============================================================================
function urlfound = searchAny(stSearchResults, query)
  urlfound = '';
  preparedQuery = [' ', strtrim(query), ' '];
  for i = 1:length(stSearchResults)
    entry = stSearchResults(i);
    if contains(entry.title, preparedQuery, 'IgnoreCase', true) || contains(entry.content, preparedQuery, 'IgnoreCase', true)
      urlfound = entry.url;
      if strcmp(urlfound,  '/./index.html') && startsWith(entry.path, 'sections/')
        urlfound = ['./', entry.path(strlength('sections/'):end), '/index.html'];
      end
      return;
    end
  end
end
%=============================================================================
function openBrowser(url, islocalhelp)
  browser = '';
  if ispc()
    browser = findWindowsBrowser();
  elseif ismac()
    browser = findMacOsBrowser();
  else
    browser = findLinuxBrowser();
  end
  
  prefix = '';
  if islocalhelp
    prefix = 'file:///';
  end
  
  if ~isempty(browser)
    if ispc()
      system(['start ', browser, ' "', prefix, url, '"']);
      return
    end
    
    if ismac()
      system(['open -a ''', browser, ''' "', prefix, url, '"']);
      return
    end
    
    if isunix()
      system([browser, ' "', prefix, url, '" &']);
      return
    end
  else
    if ispc()
      system(['start ', prefix, url]);
      return
    end
    
    if ismac()
      system(['open "', prefix, url, '"']);
      return
    end
    
    if isunix()
      system(['xdg-open ', prefix, url]);
      return
    end
  end
end
%==============================================================================
function browser = findWindowsBrowser()
  possibleBrowsers = {'chrome', 'firefox', 'opera', 'brave', 'vivaldi', 'msedge', 'iexplore', 'edge'};
  browser = '';
  for i = 1:length(possibleBrowsers)
    b = possibleBrowsers{i};
    regkey = sprintf('HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\%s.exe', b);
    try
      path = winqueryreg('HKEY_LOCAL_MACHINE','SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\chrome.exe');
      if ~isempty(path)
        browser = b;
        return;
      end
    catch
      % continue to next browser
    end
  end
end
%==============================================================================
function browser = findMacOsBrowser()
  % Try to get the default browser using plutil and jq
  cmd = ['plutil -extract LSHandlers json -o - ~/Library/Preferences/com.apple.LaunchServices/com.apple.launchservices.secure.plist | ', ...
         'jq -r ''.[] | select(.LSHandlerURLScheme=="http") | .LSHandlerRoleAll'' 2>/dev/null'];
  [status, output] = unix(cmd);
  
  if status ~= 0 || isempty(output)
    % Try fallback method with defaults command
    cmd = ['defaults read com.apple.LaunchServices/com.apple.launchservices.secure LSHandlers | ', ...
           'grep "LSHandlerURLScheme = http" -A 3 -B 2 | ', ...
           'grep LSHandlerRoleAll | ', ...
           'awk -F= ''{print $NF}'' | ', ...
           'tr -d '' ";\'' 2>/dev/null'];
    [status, output] = unix(cmd);
  end
  
  if status == 0 && ~isempty(output)
    browser_id = strtrim(output);
    
    % Map browser bundle identifiers to browser names
    if contains(browser_id, 'com.google.chrome')
      browser = 'Google Chrome';
    elseif contains(browser_id, 'org.mozilla.firefox')
      browser = 'Firefox';
    elseif contains(browser_id, 'com.apple.safari')
      browser = 'Safari';
    elseif contains(browser_id, 'com.operasoftware.opera')
      browser = 'Opera';
    elseif contains(browser_id, 'com.brave.browser')
      browser = 'Brave Browser';
    elseif contains(browser_id, 'com.microsoft.edgemac')
      browser = 'Microsoft Edge';
    elseif contains(browser_id, 'com.vivaldi.vivaldi')
      browser = 'Vivaldi';
    else
      browser = '';  % Default to empty if bundle ID not recognized
    end
    
    % If we found a browser and it exists, return it
    if ~isempty(browser) && isfolder(sprintf('/Applications/%s.app', browser))
      return;
    end
  end
  
  % Fall back to checking common browser locations
  possibleBrowsers = {'Google Chrome', ...
  'Firefox', ...
  'Safari', ...
  'Opera', ...
  'Brave Browser', ...
  'Microsoft Edge', ...
  'Vivaldi'};
  
  browser = '';
  
  % Also check user Applications folder
  for i = 1:length(possibleBrowsers)
    b = possibleBrowsers{i};
    if isfolder(sprintf('~/Applications/%s.app', b))
      browser = b;
      return;
    end
  end
  
  for i = 1:length(possibleBrowsers)
    b = possibleBrowsers{i};
    if isfolder(sprintf('/Applications/%s.app', b))
      browser = b;
      return;
    end
  end
end
%==============================================================================
function browser = findLinuxBrowser()
  [status, output] = unix('xdg-mime query default x-scheme-handler/http 2>/dev/null');
  if status == 0 && ~isempty(output)
    desktop_file = strtrim(output);
    browser_name = strrep(desktop_file, '.desktop', '');
    
    if contains(browser_name, 'chrome')
      browser = 'google-chrome';
    elseif contains(browser_name, 'chromium')
      browser = 'chromium-browser';
    elseif contains(browser_name, 'firefox')
      browser = 'firefox';
    elseif contains(browser_name, 'brave')
      browser = 'brave-browser';
    elseif contains(browser_name, 'opera')
      browser = 'opera';
    elseif contains(browser_name, 'midori')
      browser = 'midori';
    elseif contains(browser_name, 'vivaldi')
      browser = 'vivaldi';
    else
      browser = browser_name;
    end
    
    check_status = unix(['which ', browser, ' 2>/dev/null']);
    if check_status == 0
      return;
    end
  end
  
  possibleBrowsers = {'google-chrome', 'chromium-browser', 'firefox', 'opera', 'brave-browser', 'vivaldi', 'midori'};
  browser = '';
  for i = 1:length(possibleBrowsers)
    b = possibleBrowsers{i};
    status = unix(['which ', b, ' 2>/dev/null']);
    if status == 0
      browser = b;
      return;
    end
  end
end
%==============================================================================
function url = getDefaultUrl()
  defaultsConf = [modulepath('nelson','etc'), '/defaults.conf'];
  defaults = jsondecode(defaultsConf, '-file');
  url = defaults.docbook_url;
end
%==============================================================================