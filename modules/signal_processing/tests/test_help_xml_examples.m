%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
skip_testsuite(~isfile([modulepath('signal_processing'), '/help/en_US/xml/chapter.xml']), 'Help XML sources are not available in this build');
languagesToCheck = {'en_US', 'fr_FR'};
totalExamples = 0;
pattern = '<example_item_data[^>]*runnable="cli"[^>]*><!\[CDATA\[([\s\S]*?)\]\]></example_item_data>';
for languageIndex = 1:length(languagesToCheck)
  xmlPath = [modulepath('signal_processing'), '/help/', ...
    languagesToCheck{languageIndex}, '/xml'];
  files = getXmlFiles(xmlPath);
  for fileIndex = 1:length(files)
    filename = files{fileIndex};
    txt = fileread(filename);
    examples = regexp(txt, pattern, 'tokens');
    for exampleIndex = 1:length(examples)
      code = examples{exampleIndex}{1};
      try
        evalc(code);
      catch ex
        msg = sprintf(_('Help example failed in "%s" at example %d: %s'), filename, exampleIndex, ex.message);
        error(msg);
      end
    end
    totalExamples = totalExamples + length(examples);
  end
end
asserts.istrue(totalExamples >= 150);
%=============================================================================
function xmlFiles = getXmlFiles(rootDir)
  xmlFiles = {};
  entries = dir(rootDir);
  for k = 1:numel(entries)
    name = entries(k).name;
    if strcmp(name, '.') || strcmp(name, '..')
      continue;
    end
    path = [rootDir, '/', name];
    if entries(k).isdir
      xmlFiles = [xmlFiles, getXmlFiles(path)];
    else
      [~, ~, ext] = fileparts(name);
      if strcmp(ext, '.xml')
        xmlFiles{end + 1} = path;
      end
    end
  end
end
%=============================================================================
