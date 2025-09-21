%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function index_help_for_language(destination_dir, lang, modules_help_list, with_main)
  if ~isdir(destination_dir)
    return;
  end

  list_summary_files = {};
  for i = 1:numel(modules_help_list)
    module_name = modules_help_list{i};
    summary_file = fullfile(destination_dir, module_name, 'help_toc_summary.xml');
    if isfile(summary_file)
      list_summary_files = [list_summary_files; summary_file];
    else
      summary_file = fullfile(destination_dir, '../', getdefaultlanguage(), '/', module_name, '/help_toc_summary.xml');
      if isfile(summary_file)
        list_summary_files = [list_summary_files; summary_file];
      end
    end
  end
  if with_main
      main_summary_file = [nelsonroot() '/modules/main/help/' lang '/xml/summary_header.xml'];
      if ~isfile(main_summary_file)
          main_summary_file = [nelsonroot() '/modules/main/help/' getdefaultlanguage() '/xml/summary_header.xml'];
      end
      list_summary_files =[main_summary_file; list_summary_files];
  end

  
  xmldocmergesummary([destination_dir],list_summary_files);
  xmltransform([destination_dir '/help_summary.xml'], [nelsonroot() '/modules/help_tools/resources/nelson_summary2html.xslt'], ...
  [destination_dir '/summary.html']);

  xmltransform([destination_dir '/help_summary.xml'], [nelsonroot() '/modules/help_tools/resources/nelson_toc2html.xslt'], ...
  [destination_dir '/toc.html']);

end
%=============================================================================
