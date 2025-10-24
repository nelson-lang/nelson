%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = buildhelpjson(varargin)
  % BUILDHELPJSON Build help JSON files from XML documentation.
  %
  narginchk(0, 0);
  nargoutchk(0, 0);
  varargout = {};
  run([nelsonroot() '/modules/' 'modules.m']);
  funcList = @(x) x{1};
  modules_help_list = string(cellfun(funcList, modules_list, 'UniformOutput', false));
  for lang = getavailablelanguages()'
    destination_json_directory = [nelsonroot() '/modules/help_tools/help/'];
    if ~isdir(destination_json_directory)
      msg = sprintf(_('Destination directory for help json files not found: %s'), destination_json_directory);
      error(msg);
    end
    msg = sprintf(_('Building help json files for language: %s\n'), lang{1});
    fprintf(msg);
    temp_dir = tempname();
    mkdir(temp_dir);
    main_st = struct();
    for m = modules_help_list(:)'
      xml_module_path = [nelsonroot() '/modules/' m{1}, '/help/', lang{1}, '/xml/'];
      if isdir(xml_module_path)
        xml_files = dir([xml_module_path, '*.xml']);
        if isempty(xml_files)
          continue;
        end
        msg = sprintf(_('help "%s" (%s) generated.'), m{1}, lang{1});
        fprintf(['   ', msg, '\n']);
        for f = xml_files'
          if ~(ismacro(f.name(1:end-4)) || isbuiltin(f.name(1:end-4)))
            continue;
          end
          xml_filename = [xml_module_path, f.name];
          json_filename = [temp_dir, '/', m{1}, '_', lang{1}, '_', f.name(1:end-4), '.json'];
          xmltransform(xml_filename, ...
            [nelsonroot() '/modules/help_tools/resources/nelson_json.xslt'], ...
            json_filename);
          st = jsondecode(json_filename, '-file');
          names = fieldnames(st);
          lower_names = lower(names);
          st = renameStructField(st, names, lower_names);
          rmfile(json_filename);
          if (isfield(st, 'keyword_alias'))
            aliases = st.keyword_alias;
            for alias = aliases(:)'
              main_st.(alias{1}) = st;
            end
          end
          main_st.(f.name(1:end-4)) = st;
        end
      end
    end
    content = jsonencode(main_st);
    dst_json_filename = fullfile(destination_json_directory, ['nelson_help_', lang{1}, '.json']);
    filewrite(dst_json_filename, content);
    rmdir(temp_dir, 's');
  end
end