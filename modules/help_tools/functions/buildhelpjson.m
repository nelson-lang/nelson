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
  if ~isempty(getenv('CONDA_PREFIX')) && ismac()
    warning('Nelson:help_tools:condaNotSupported', 'Building help files is not allowed in a conda environment. libxslt causes issues on macOS.');
    return;
  end
  narginchk(0, 0);
  nargoutchk(0, 0);
  varargout = {};
  modules_help_list = get_builtin_help_modules();
  for lang = getavailablelanguages()'
    destination_json_directory = [nelsonroot() '/modules/help_tools/help/'];
    if ~isdir(destination_json_directory)
      error('Nelson:buildhelpjson:destinationNotFound', 'Destination directory for help json files not found: %s', destination_json_directory);
    end
    msg = [sprintf(_('Building help json files for language: %s'), lang{1}), sprintf('\n')];
    fprintf(msg);
    temp_dir = tempname();
    mkdir(temp_dir);
    main_st = struct();
    nelson_json_xslt = [modulepath('help_tools'), '/resources/nelson_json.xslt'];
    for m = modules_help_list(:)'
      xml_module_path = [nelsonroot() '/modules/' m{1}, '/help/', lang{1}, '/xml/'];
      if isdir(xml_module_path)
        xml_files = dir([xml_module_path, '/*.xml'], '-s');
        if isempty(xml_files)
          continue;
        end
        msg = sprintf(_('help "%s" (%s) generated.'), m{1}, lang{1});
        fprintf(['   ', msg, '\n']);
        for f = xml_files'
          if strcmp(f.name, 'chapter.xml')
            continue;
          end
          basename = f.name(1:end-4);
          xml_filename = [f.folder, '/', f.name];
          % Index a page when its file name is a function/builtin, or when the
          % page declares a keyword_alias. nflow block help pages expose a
          % "block:<type>" alias (used to open a block's help from nflow) but
          % their file name is not a Nelson function, so the function-only
          % filter used to drop every block from the search index.
          if ~(ismacro(basename) || isbuiltin(basename) || pageHasKeywordAlias(xml_filename))
            continue;
          end
          json_filename = [temp_dir, '/', m{1}, '_', lang{1}, '_', basename, '.json'];
          xmltransform(xml_filename, nelson_json_xslt, json_filename);
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
          main_st.(basename) = st;
        end
      end
    end
    content = jsonencode(main_st);
    dst_json_filename = fullfile(destination_json_directory, ['nelson_help_', lang{1}, '.json']);
    filewrite(dst_json_filename, content);
    rmdir(temp_dir, 's');
  end
end
%=============================================================================
function tf = pageHasKeywordAlias(xml_filename)
  tf = false;
  try
    content = fileread(xml_filename);
    tf = contains(content, '<keyword_alias>');
  catch
    tf = false;
  end
end
%=============================================================================
