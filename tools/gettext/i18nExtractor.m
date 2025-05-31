%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function i18nExtractor(destinationDir)
  tic();
  if nargin < 1 || isempty(destinationDir)
    destinationDir = tempdir(); % default if none provided
  end

  % Ensure trailing file separator
  if ~endsWith(destinationDir, filesep)
    destinationDir = [destinationDir, filesep];
  end

  if ispc()
    XGETTEXT = ['"', nelsonroot(), '/tools/gettext/bin/xgettext.exe', '"'];
    MSGCAT = ['"', nelsonroot(), '/tools/gettext/bin/msgcat.exe', '"'];
  else
    XGETTEXT = 'xgettext';
    MSGCAT = 'msgcat';
  end

  DOMAIN = 'nelson';
  TARGETDIR = [destinationDir, DOMAIN, '_pot'];

  if isfolder(TARGETDIR)
    rmdir(TARGETDIR, 's');
  end
  mkdir(TARGETDIR);

  process_files('*.m', '--language=Python -k --keyword=_', XGETTEXT, DOMAIN, TARGETDIR);
  process_files({'*.c', '*.h', '*.cpp', '*.cxx', '*.hpp', '*.hxx'}, ...
    '--language=C -k --keyword=dgettext --keyword=_ --keyword=_W --keyword=TR', ...
    XGETTEXT, DOMAIN, TARGETDIR);

  merge_pots(MSGCAT, DOMAIN, TARGETDIR);

  finalize_json_files(DOMAIN);
  toc();
  if nargin < 1 || isempty(destinationDir)
    exit('force');
  end
end
%=============================================================================
function process_files(exts, xgettext_lang_opts, xgettext, domain, targetdir)
  if ischar(exts)
    exts = {exts};
  end

  % Collect matching files recursively under nelsonroot()
  all_files = [];
  for k = 1:length(exts)
    all_files = [all_files; dir(fullfile(nelsonroot(), exts{k}), '-s')];
  end

  % Extract unique parent folders
  folders = unique_if_exists(all_files, 'folder');

  commands = [];
  for i = 1:length(folders)
    folder = folders{i};

    % Get all matching files in current folder (non-recursively)
    files = [];
    for k = 1:length(exts)
      files = [files; dir(fullfile(folder, exts{k}))];
    end
    if isempty(files)
      continue;
    end

    % Normalize relative path to build output filenames
    relpath = sanitize_path(strrep(folder, nelsonroot(), ''));
    listFile = fullfile(targetdir, [domain, '_', relpath, '_list.txt']);
    potFile = fullfile(targetdir, [domain, '_', relpath, '.pot']);

    % Write list of input files for xgettext
    write_file_list(listFile, files);

    % Construct xgettext command
    xgettext_opts = ['--omit-header ', xgettext_lang_opts, ' -force-po --from-code=UTF-8'];
    cmd = sprintf('%s %s -d %s -o "%s" -f "%s"', ...
      xgettext, xgettext_opts, domain, potFile, listFile);

    commands = [commands, string(cmd)];
  end
  % Run commands in parallel and report errors
  [r, errmsg] = system(commands);
  if (any(r ~= 0) && any(errmsg ~= ""))
    errmsg(errmsg == "") = [];
    if isempty(errmsg)
      errmsg = 'Unknown error occurred during xgettext extraction.';
    else
      errmsg = string(errmsg(1));
    end 
    fprintf('Error during xgettext extraction:\n%s\n', errmsg);
  end
end
%=============================================================================
function folders = unique_if_exists(files, fieldname)
  if isfield(files, fieldname)
    folders = unique({files.(fieldname)});
  else
    folders = {};
  end
end
%=============================================================================
function relpath = sanitize_path(path)
  % Replace path separators with underscores
  relpath = strrep(path, '/', '_');
  relpath = strrep(relpath, '\', '_');
  if startsWith(relpath, '_')
    relpath = relpath(2:end);
  end
end
%=============================================================================
function write_file_list(filename, files)
  fid = fopen(filename, 'wt');
  if fid == -1
    error('Cannot open file for writing: %s', filename);
  end
  for k = 1:length(files)
    fprintf(fid, '%s\n', fullfile(files(k).folder, files(k).name));
  end
  fclose(fid);
end
%=============================================================================
function merge_pots(msgcat, domain, targetdir)
  potFiles = dir(fullfile(targetdir, '*.pot'));
  if isempty(potFiles)
    return;
  end

  % Write a list of all generated .pot files
  listPotsFile = fullfile(targetdir, [domain, '_all_pots_list.txt']);
  write_file_list(listPotsFile, potFiles);

  % Output file for the merged pot
  finalPotFile = fullfile(targetdir, [domain, '_final.pot']);
  msgcat_opts = '--lang=en_US --force-po --no-location';
  cmd = sprintf('%s %s -o "%s" -f "%s"', msgcat, msgcat_opts, finalPotFile, listPotsFile);

  [r, errmsg] = system(cmd);
  if r ~= 0 && ~isempty(errmsg)
    fprintf('Error during msgcat merge:\n%s\n', errmsg);
  end

  % Copy merged POT to locale directory
  copyfile(finalPotFile, [nelsonroot(), '/locale/nelson.pot'], 'f');
end
%=============================================================================
function finalize_json_files(domain)
  potfile = [nelsonroot(), '/locale/nelson.pot'];
  jsonUSfile = [nelsonroot(), '/locale/nelson-en_US.json'];
  jsonFRfile = [nelsonroot(), '/locale/nelson-fr_FR.json'];

  % Convert POT to base JSON (en_US)
  i18nHelpers('convert', potfile, jsonUSfile);

  % Merge with existing French translation
  i18nHelpers('merge', jsonUSfile, jsonFRfile);

  % Sort both JSON files to ensure consistent structure
  i18nHelpers('sort', jsonUSfile, jsonUSfile);
  i18nHelpers('sort', jsonFRfile, jsonFRfile);
end
%=============================================================================
