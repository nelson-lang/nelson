%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
skip_testsuite(~isfile([modulepath('data_analysis'), '/help/en_US/xml/chapter.xml']), 'Help XML sources are not available in this build');
modulesToCheck = {'categorical', 'data_analysis'};
languagesToCheck = {'en_US', 'fr_FR'};
skip_testsuite(isempty(which('fft')), 'Some data_analysis help examples require fft.');
totalExamples = 0;
for moduleIndex = 1:length(modulesToCheck)
  for languageIndex = 1:length(languagesToCheck)
    xmlPath = [modulepath(modulesToCheck{moduleIndex}), '/help/', ...
      languagesToCheck{languageIndex}, '/xml'];
    files = dir([xmlPath, '/*.xml']);
    for fileIndex = 1:length(files)
      filename = [xmlPath, '/', files(fileIndex).name];
      examples = localCliExamples(filename);
      for exampleIndex = 1:length(examples)
        localRunExample(examples{exampleIndex}, filename, exampleIndex);
      end
      totalExamples = totalExamples + length(examples);
    end
  end
end
asserts.istrue(totalExamples >= 80);
%=============================================================================
function examples = localCliExamples(filename)
  txt = fileread(filename);
  pattern = '<example_item_data[^>]*runnable="cli"[^>]*><!\[CDATA\[([\s\S]*?)\]\]></example_item_data>';
  tokens = regexp(txt, pattern, 'tokens');
  examples = cell(1, length(tokens));
  for k = 1:length(tokens)
    examples{k} = tokens{k}{1};
  end
end
%=============================================================================
function localRunExample(code, filename, index)
  try
    evalc(code);
  catch ex
    error(_('Help example failed in "%s" at example %d: %s'), filename, index, ex.message);
  end
end
%=============================================================================
