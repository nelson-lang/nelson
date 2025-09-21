%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function buildhelp(varargin)
  % build Nelson help files
  % buildhelp() build help of Nelson
  % buildhelp(module_name) build help of a module loaded in Nelson
  %
  narginchk(0, 1);
  TMP = tempname();
  if (nargin() == 0)
    modules_help_list = get_modules_help_list();
    modules_help_list_with_main = [modules_help_list; "main"];

    for lang = getavailablelanguages()'
      tmplang = [TMP, '/',lang{1}];
      mkdir(tmplang);
      for m = modules_help_list_with_main'
        module_path = [nelsonroot() '/modules/' m{1}];
        build_help_from_path(m{1}, module_path, lang{1}, tmplang, true);
      end
      index_help_for_language(tmplang, lang{1}, modules_help_list, true);
      repackage_main_help(tmplang, lang{1});
    end
    rmdir(TMP, 's');
    return;
  end
  module = varargin{1};
  module_path = modulepath(module);
  for lang = getavailablelanguages()'
    build_help_from_path(module, module_path, lang{1}, TMP, true);
  end
end
%=============================================================================
function repackage_main_help(destination_dir, lang)
  destination_dir_main = [destination_dir, '/', 'main'];
  copyfile([destination_dir, '/' ,'index.js'], [destination_dir_main, '/', 'index.js'])
  copyfile([destination_dir, '/' ,'index.json'], [destination_dir_main, '/', 'index.json'])
  copyfile([destination_dir, '/' ,'summary.html'], [destination_dir_main, '/', 'summary.html'])
  copyfile([destination_dir, '/' ,'toc.html'], [destination_dir_main, '/', 'toc.html'])
  copyfile([destination_dir, '/' ,'help_summary.xml'], [destination_dir_main, '/', 'help_summary.xml'])

  help_archive_name = [nelsonappid(), '.modules.main.help', '.', lang, '.nhz'];
  destination_archive =  [nelsonroot(), '/modules/main/help/', help_archive_name];

  zip(destination_archive, destination_dir_main);
end
%=============================================================================