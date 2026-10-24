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
  dryRun = strcmp(getenv('NELSON_DOC_DRY_RUN'), 'TRUE');
  % The browser-backed WebAssembly terminal has its own public mode, but it
  % still provides the same documentation viewer as desktop hosts. Keep the
  % restriction for engine-only/native basic terminals while letting the
  % WebAssembly frontend resolve local and remote documentation normally.
  if contains(getnelsonmode(), {'BASIC_ENGINE', 'BASIC_TERMINAL'}) && ~iswasm() && ~dryRun
    warning('Nelson:doc:notAvailableInMode', 'Documentation is not available in this mode. See "help" command for more information.');
    return
  end
  mainUrl = docroot();
  islocalhelp = strcmp(mainUrl, '.');
  ver_number = version('-number');
  version_string = sprintf('%d.%d.%d', ver_number(1), ver_number(2), ver_number(3));

  if islocalhelp
    version_string = sprintf('%d.%d.%d', ver_number(1), ver_number(2), ver_number(3));
    mainUrl = getLocalHelpCacheDir(version_string);
    if endsWith(mainUrl, '/')
      mainUrl = mainUrl(1:end - 1);
    end
    if ~isdir(mainUrl)
      warning('Nelson:doc:localHelpNotInstalled:localHelpNotInstalledOpenWeb', 'Local help not installed. Open web help instead.');
      islocalhelp = false;
      mainUrl = getDefaultUrl();
    end
  else
    version_string = sprintf('v%d.%d.%d', ver_number(1), ver_number(2), ver_number(3));
  end
  txt = joinText(varargin);
  language = getlanguage();
  [stSearchResults, baseUrl] = getSearchBase(mainUrl, language, version_string, islocalhelp);
  url = buildUrl(baseUrl, txt, islocalhelp, stSearchResults, dryRun);
  if dryRun
    disp(url);
    return;
  end
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
      error('Nelson:doc:localHelpNotInstalled', 'Local help not installed.');
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
  if isfile(cacheFile) && ~isCacheFileExpired(cacheFile)
    jsonFile = cacheFile;
    return;
  end
  jsonSearchUrl = [baseUrl, '/', 'index.json'];
  try
    jsonFile = websave(cacheFile, jsonSearchUrl, weboptions('ContentType','json'));
  catch
    % keep serving a stale index rather than nothing when offline.
    if isfile(cacheFile)
      jsonFile = cacheFile;
    else
      jsonFile = '';
    end
  end
end
%=============================================================================
function tf = isCacheFileExpired(cacheFile)
  CACHE_LIFETIME_IN_DAYS = 7;
  tf = true;
  try
    info = dir(cacheFile);
    if ~isempty(info) && info(1).datenum > 0
      tf = (now() - info(1).datenum) > CACHE_LIFETIME_IN_DAYS;
    end
  catch
    tf = true;
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
function url = buildUrl(baseUrl, txt, islocalhelp, stSearchResults, dryRun)
  if isempty(txt)
    url = [baseUrl, '/', 'index.html'];
    return;
  end
  urlfound = '';
  % NFlow supplies the canonical relative page for a block when its palette
  % manifest knows the library directory. Keep this override in doc.m so the
  % native, WebView, and WebAssembly hosts all use the same URL construction.
  blockPage = getenv('NELSON_DOC_BLOCK_PAGE');
  if startsWith(txt, 'block:') && ~isempty(blockPage)
    urlfound = blockPage;
  end
  if isempty(urlfound) && (ismacro(txt) || isbuiltin(txt))
    urlfound = searchMacroOrBuiltin(stSearchResults, txt);
  end
  if isempty(urlfound) && ismodule(txt)
    urlfound = searchModule(stSearchResults, txt);
  end
  if isempty(urlfound)
    urlfound = searchAny(stSearchResults, txt);
  end
  % Portable targets may not provide the network helper used to download the
  % remote index. Resolve a builtin or macro from its owning module so the
  % same viewer URL can still open the exact page instead of a search page.
  if isempty(urlfound) && ~islocalhelp
    module = findHelpModule(txt);
    if ~isempty(module)
      urlfound = ['./', module, '/', txt, '.html'];
    end
  end
  if ~isempty(urlfound)
    url = buildViewerUrl(baseUrl, 'open', urlfound, islocalhelp, dryRun);
  else
    url = buildViewerUrl(baseUrl, 'search', txt, islocalhelp, dryRun);
  end
end
%=============================================================================
function url = buildViewerUrl(baseUrl, key, value, islocalhelp, dryRun)
  if islocalhelp
    url = buildLocalViewerLauncherUrl(baseUrl, key, value, dryRun);
  else
    url = [baseUrl, '/', 'index.html?', key, '=', encodeUrlComponent(value)];
  end
end
%=============================================================================
function url = buildLocalViewerLauncherUrl(baseUrl, key, value, dryRun)
  target = ['file://', baseUrl, '/', 'index.html#', key, '=', encodeUrlComponent(value)];
  if dryRun
    url = target;
    return;
  end
  url = getenv('NELSON_DOC_OPEN_PATH');
  if isempty(url)
    url = [tempdir(), 'nelson_doc_open.html'];
  end
  html = ['<!DOCTYPE html>', newline(), ...
  '<html><head><meta charset="utf-8">', newline(), ...
  '<meta http-equiv="refresh" content="0; url=', target, '">', newline(), ...
  '<script>window.location.replace(', jsonencode(target), ');</script>', newline(), ...
  '</head><body><a href="', target, '">Open documentation</a></body></html>', newline()];
  try
    filewrite(url, html);
  catch
    url = [baseUrl, '/', 'index.html#', key, '=', encodeUrlComponent(value)];
  end
end
%=============================================================================
function encoded = encodeUrlComponent(value)
  if isbuiltin('urlencode') || ismacro('urlencode')
    encoded = urlencode(value);
    return
  end
  codepoints = double(char(value));
  bytes = [];
  for k = 1:length(codepoints)
    codepoint = codepoints(k);
    if codepoint <= 127
      bytes = [bytes, codepoint];
    elseif codepoint <= 2047
      bytes = [bytes, 192 + floor(codepoint / 64), 128 + mod(codepoint, 64)];
    elseif codepoint <= 65535
      bytes = [bytes, 224 + floor(codepoint / 4096), ...
        128 + mod(floor(codepoint / 64), 64), 128 + mod(codepoint, 64)];
    else
      bytes = [bytes, 240 + floor(codepoint / 262144), ...
        128 + mod(floor(codepoint / 4096), 64), ...
        128 + mod(floor(codepoint / 64), 64), 128 + mod(codepoint, 64)];
    end
  end
  encoded = '';
  for k = 1:length(bytes)
    byte = double(bytes(k));
    if (byte >= 48 && byte <= 57) || (byte >= 65 && byte <= 90) || ...
        (byte >= 97 && byte <= 122) || any(byte == [45, 46, 95, 126])
      encoded = [encoded, char(byte)];
    else
      encoded = [encoded, sprintf('%%%02X', byte)];
    end
  end
end
%=============================================================================
function module = findHelpModule(query)
  module = '';
  try
    location = which(query);
  catch
    return;
  end
  static_module = regexp(location, 'static:nls([A-Za-z0-9_]+)_builtin', 'tokens', 'once');
  if ~isempty(static_module)
    module = [lower(static_module{1}(1)), static_module{1}(2:end)];
    return;
  end
  module_path = regexp(location, '/modules/([A-Za-z0-9_]+)/', 'tokens', 'once');
  if ~isempty(module_path)
    module = module_path{1};
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
  preparedQuery = strtrim(query);
  if isempty(preparedQuery)
    return;
  end
  % match on a word boundary, so that 'plot' also matches 'plot,' or a title
  % which is exactly 'plot' (the previous space padding matched neither).
  pattern = ['\b', regexptranslate('escape', preparedQuery), '\b'];
  for i = 1:length(stSearchResults)
    entry = stSearchResults(i);
    % Keep aliases (notably nflow's block:<type> aliases) exact. They are
    % lookup-only entries in index.json and may contain punctuation such as
    % ':', which is not a word boundary for the regular-expression fallback.
    exactTitle = isfield(entry, 'title') && strcmp(entry.title, preparedQuery);
    exactAlias = false;
    if isfield(entry, 'keyword_alias')
      aliases = entry.keyword_alias;
      if ischar(aliases) || isstring(aliases)
        aliases = {char(aliases)};
      end
      if iscell(aliases)
        exactAlias = any(strcmp(aliases, preparedQuery));
      end
    end
    if exactTitle || exactAlias || ~isempty(regexpi(entry.title, pattern, 'once')) || ~isempty(regexpi(entry.content, pattern, 'once'))
      urlfound = entry.url;
      if strcmp(urlfound,  '/./index.html') && startsWith(entry.path, 'sections/')
        urlfound = ['./', entry.path(strlength('sections/'):end), '/index.html'];
      end
      return;
    end
  end
end
%=============================================================================
function help_dir = getLocalHelpCacheDir(version_string)
  help_dir = getenv('NELSON_HELP_CACHE_DIR');
  if ~isempty(help_dir)
    help_dir = ensureTrailingFileSeparator(help_dir);
  elseif ~isempty(getenv('NELSON_PREFERENCES_PATH'))
    help_dir = [prefdir(), '/help/'];
  else
    help_dir = [userdir(), '/Nelson/', version_string, '/help/'];
  end
end
%=============================================================================
function path = ensureTrailingFileSeparator(path)
  if ~endsWith(path, '/')
    path = [path, '/'];
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
    prefix = 'file://';
  end

  if ~isempty(browser)
    if ispc()
      system(['start ', browser, ' "', prefix, url, '"']);
      return
    end

    if ismac()
      status = system(['open -a ''', browser, ''' "', prefix, url, '" 2>/dev/null']);
      if status == 0
        return
      end
      browser = '';
    end

    if isunix()
      system([browser, ' "', prefix, url, '" &']);
      return
    end
  else
    if ispc()
      % the empty title argument is required, otherwise 'start' consumes the
      % quoted url as the console window title.
      system(['start "" "', prefix, url, '"']);
      return
    end

    if ismac()
      status = system(['open "', prefix, url, '" 2>/dev/null']);
      if status ~= 0
        warning('Nelson:doc:cannotOpenUrl', 'Unable to open documentation URL: %s', [prefix, url]);
      end
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
  % On Windows, 'start' already honours the browser the user registered as
  % default: do not force a particular one.
  browser = '';
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

    % If we found a browser and LaunchServices can resolve it, return it.
    if ~isempty(browser) && unix(['open -Ra ''', browser, ''' >/dev/null 2>&1']) == 0
      return;
    end
    browser = '';
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
    if unix(['open -Ra ''', b, ''' >/dev/null 2>&1']) == 0
      browser = b;
      return;
    end
  end

  for i = 1:length(possibleBrowsers)
    b = possibleBrowsers{i};
    if unix(['open -Ra ''', b, ''' >/dev/null 2>&1']) == 0
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
