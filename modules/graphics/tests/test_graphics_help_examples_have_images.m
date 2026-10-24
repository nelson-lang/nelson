%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
testRoot = fileparts(mfilename('fullpathext'));
helpRoot = [fileparts(testRoot), '/help'];
languages = help_languages(helpRoot);
for k = 1:numel(languages)
  check_help_examples_have_images(helpRoot, languages{k});
end
%=============================================================================
function check_help_examples_have_images(helpRoot, language)
  xmlRoot = [helpRoot, '/', language, '/xml'];
  xmlFiles = list_xml_files(xmlRoot);
  for k = 1:numel(xmlFiles)
    filename = xmlFiles{k};
    if endsWith(filename, '/chapter.xml')
      continue;
    end
    helpText = fileread(filename);
    check_declared_example_images_exist(filename, helpText);
  end
end
%=============================================================================
function check_declared_example_images_exist(filename, helpText)
  starts = strfind(helpText, '<example_item_img');
  for k = 1:numel(starts)
    tagText = helpText(starts(k):min(starts(k) + 512, length(helpText)));
    src = extract_image_src(tagText);
    asserts.istrue(~isempty(src), filename);
    if is_generated_image(tagText)
      continue;
    end
    imageFilename = normalize_help_image_path(fileparts(filename), src);
    asserts.istrue(isfile(imageFilename), imageFilename);
  end
end
%=============================================================================
function tf = is_generated_image(tagText)
  tf = contains(tagText, 'generate="true"');
end
%=============================================================================
function src = extract_image_src(tagText)
  src = '';
  key = 'src="';
  p1 = strfind(tagText, key);
  if ~isempty(p1)
    p1 = p1(1) + length(key);
    p2 = strfind(tagText(p1:end), '"');
    if ~isempty(p2)
      src = tagText(p1:p1 + p2(1) - 2);
    end
  end
end
%=============================================================================
function imageFilename = normalize_help_image_path(baseFolder, src)
  imageFilename = strrep([baseFolder, '/', src], '\', '/');
  parts = strsplit(imageFilename, '/');
  normalized = {};
  for k = 1:numel(parts)
    part = parts{k};
    if isempty(part) || strcmp(part, '.')
      continue;
    end
    if strcmp(part, '..')
      normalized(end) = [];
    else
      normalized{end + 1} = part;
    end
  end
  if startsWith(imageFilename, '/')
    imageFilename = ['/', strjoin(normalized, '/')];
  else
    imageFilename = strjoin(normalized, '/');
  end
end
%=============================================================================
function languages = help_languages(helpRoot)
  languages = {};
  entries = dir(helpRoot);
  for k = 1:numel(entries)
    name = entries(k).name;
    if strcmp(name, '.') || strcmp(name, '..') || ~entries(k).isdir
      continue;
    end
    if isfolder([helpRoot, '/', name, '/xml'])
      languages{end + 1} = name;
    end
  end
end
%=============================================================================
function xmlFiles = list_xml_files(folder)
  xmlFiles = {};
  entries = dir(folder);
  for k = 1:numel(entries)
    name = entries(k).name;
    if strcmp(name, '.') || strcmp(name, '..')
      continue;
    end
    entryPath = [folder, '/', name];
    if entries(k).isdir
      xmlFiles = [xmlFiles, list_xml_files(entryPath)];
    elseif endsWith(name, '.xml')
      xmlFiles{end + 1} = entryPath;
    end
  end
end
%=============================================================================
