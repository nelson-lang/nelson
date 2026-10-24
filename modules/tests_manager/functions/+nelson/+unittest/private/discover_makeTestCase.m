%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function testCase = discover_makeTestCase(filename, parsedTags)
  [~, name, ~] = fileparts(filename); %#ok<NLS0004>
  if nargin < 2
    options = parsetags(filename);
  else
    options = parsedTags;
  end
  testCase = struct();
  testCase.id = createGUID();
  testCase.module = discover_moduleNameFromFile(filename);
  testCase.file = filename;
  testCase.name = name;
  testCase.kind = discover_caseKind(name);
  testCase.tags = discover_tagNames(options);
  testCase.mode = discover_executionMode(options);
  testCase.resources = discover_resourcesFromOptions(options);
  testCase.timeout = discover_defaultTimeout(testCase.kind, option_timeout(options));
  testCase.weight = options.weight;
end
