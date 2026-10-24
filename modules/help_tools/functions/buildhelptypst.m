%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function buildhelptypst(varargin)
  % build Nelson help files as Typst sources
  % buildhelptypst(destdir) build help of Nelson
  % buildhelptypst(destdir, module_name) build help of a module loaded in Nelson
  %
  % Layout: <destdir>/<locale>/main.typ includes <destdir>/<locale>/<module>/main.typ
  % for every generated module; compile either file with the typst compiler.
  narginchk(1, 2);
  destinationdir = varargin{1};
  if nargin() == 1
    main_title = 'Nelson';
    % string array of builtin module names
    modules_help_list = cellstr(get_builtin_help_modules());
    module_paths = {};
    for k = 1:length(modules_help_list)
      module_paths{end + 1} = [nelsonroot(), '/modules/', modules_help_list{k}];
    end
  else
    main_title = varargin{2};
    modules_help_list = {varargin{2}};
    module_paths = {modulepath(varargin{2})};
  end
  locales = {};
  modules = {};
  for k = 1:length(modules_help_list)
    l = buildHelpFromPath(destinationdir, modules_help_list{k}, module_paths{k});
    locales = [locales, l];
    if ~isempty(l)
      modules{end + 1} = modules_help_list{k};
    end
  end
  locales = unique(locales);
  front_matter = (nargin() == 1);
  if front_matter
    helpForNelsonOnly(destinationdir, locales);
  end
  buildMainDocument(destinationdir, locales, modules, main_title, front_matter);
end
%=============================================================================
function helpForNelsonOnly(destinationdir, locales)
  % Home page, getting started, changelogs and licenses of the whole manual:
  % hand-written markdown pages converted to Typst (same sources as
  % buildhelpmd). Labels <main:<name>> allow cross-references with nlink.
  helpers = [modulepath('help_tools'), '/resources/nelson_help.typ'];
  for k = getavailablelanguages()(:)'
    locale = k{1}(1:2);
    if ~any(strcmp(locales, locale))
      continue;
    end
    dir_dst = [destinationdir, '/', locale];
    dir_src = [nelsonroot(), '/modules/main/help/', k{1}, '/md'];
    if ~isdir(dir_src)
      continue;
    end
    copyfile(helpers, [dir_dst, '/nelson_help.typ']);
    if isfile([dir_src, '/banner_homepage.png'])
      copyfile([dir_src, '/banner_homepage.png'], [dir_dst, '/banner_homepage.png']);
    end
    convertMarkdownPage([dir_src, '/homepage.md'], [dir_dst, '/homepage.typ'], 'main:homepage', 'nelson_help.typ');
    convertMarkdownPage([dir_src, '/getting_started.md'], [dir_dst, '/getting_started.typ'], 'main:getting_started', 'nelson_help.typ');
    mkdir([dir_dst, '/changelogs']);
    for c = changelogFiles()
      convertMarkdownPage([nelsonroot(), '/', c{1}], [dir_dst, '/changelogs/', strrep(c{1}, '.md', '.typ')], ['main:', strrep(c{1}, '.md', '')], '../nelson_help.typ');
    end
    mkdir([dir_dst, '/license']);
    for c = {'license.md', 'lgpl-3.0.md', 'gpl-3.0.md'}
      src = [dir_src, '/', c{1}];
      if ~isfile(src)
        src = [nelsonroot(), '/', c{1}];
      end
      convertMarkdownPage(src, [dir_dst, '/license/', strrep(c{1}, '.md', '.typ')], ['main:', strrep(c{1}, '.md', '')], '../nelson_help.typ');
    end
  end
end
%=============================================================================
function files = changelogFiles()
  files = {'CHANGELOG.md', 'CHANGELOG-1.x.x.md', 'CHANGELOG-0.7.x.md', 'CHANGELOG-0.6.x.md', ...
           'CHANGELOG-0.5.x.md', 'CHANGELOG-0.4.x.md', 'CHANGELOG-0.3.x.md', 'CHANGELOG-0.2.x.md', 'CHANGELOG-0.1.x.md'};
end
%=============================================================================
function convertMarkdownPage(src, dst, label, helpers)
  if ~isfile(src)
    return;
  end
  typst = markdown_to_typst(fileread(src), label, helpers);
  filewrite(dst, typst);
end
%=============================================================================
function locales = buildHelpFromPath(destinationdir, module, module_path)
  locales = {};
  for k = getavailablelanguages()(:)'
    locale = k{1}(1:2);
    src = [module_path, '/help/', k{1}, '/xml'];
    if ~isdir(src)
      continue;
    end
    dstbuild = [destinationdir, '/', locale, '/', module, '/'];
    if isdir(dstbuild)
      [res, msg] = rmdir(dstbuild, 's');
      if ~res
        error('Nelson:buildhelptypst:removeDirectoryFailed', msg);
      end
    end
    [res, msg] = mkdir(dstbuild);
    if ~res
      error('Nelson:buildhelptypst:makeDirectoryFailed', msg);
    end
    % index every module of this language so that cross-module links resolve
    index_dirs = help_xml_index_directories(k{1}, {src});
    [status, msg] = xmldoctotypst(src, dstbuild, module, true, index_dirs);
    if ~status
      error('Nelson:buildhelptypst:generationFailed', msg);
    end
    disp(sprintf(_("help '%s' (%s) generated."), module, k{1}));
    locales{end + 1} = locale;
  end
end
%=============================================================================
function buildMainDocument(destinationdir, locales, modules, main_title, front_matter)
  for l = locales(:)'
    locale_dir = [destinationdir, '/', l{1}];
    % The document style (fonts, headings, code boxes, tables, footer) is the
    % nelson-style show rule of nelson_help.typ, copied next to main.typ.
    copyfile([modulepath('help_tools'), '/resources/nelson_help.typ'], [locale_dir, '/nelson_help.typ']);
    content = {'#import "nelson_help.typ": nelson-style'; ...
               ['#set document(title: "', strrep(main_title, '"', '\"'), '")']; ...
               ['#show: nelson-style.with(lang: "', l{1}, '")']; ...
               ''};
    % Same order as the markdown manual: home page, getting started, modules,
    % changelogs, licenses, and the table of contents at the end.
    includes = {};
    if front_matter
      includes = [includes, {'homepage.typ', 'getting_started.typ'}];
    end
    for m = modules(:)'
      includes{end + 1} = [m{1}, '/main.typ'];
    end
    if front_matter
      for c = changelogFiles()
        includes{end + 1} = ['changelogs/', strrep(c{1}, '.md', '.typ')];
      end
      includes = [includes, {'license/license.typ', 'license/lgpl-3.0.typ', 'license/gpl-3.0.typ'}];
    end
    first = true;
    for f = includes
      if isfile([locale_dir, '/', f{1}])
        if ~first
          content{end + 1} = '#pagebreak(weak: true)';
        end
        content{end + 1} = ['#include "', f{1}, '"'];
        first = false;
      end
    end
    content{end + 1} = '#pagebreak(weak: true)';
    content{end + 1} = '#outline(depth: 2)';
    content{end + 1} = '';
    filewrite([locale_dir, '/main.typ'], content);
  end
end
%=============================================================================
