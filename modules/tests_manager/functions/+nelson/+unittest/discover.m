%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function suite = discover(varargin)
  [targets, kind] = discover_parseDiscoverArguments(varargin); %#ok<NLS0004>
  files = discover_discoverFiles(targets, kind);
  caseList = cell(length(files), 1);
  parsedTags = parsetags(files); %#ok<NLS0004>
  for k = 1:length(files)
    caseList{k} = discover_makeTestCase(files{k}, parsedTags(k)); %#ok<NLS0004>
  end
  cases = struct([]);
  for k = 1:length(caseList)
    cases = [cases; caseList{k}];
  end
  suite = struct();
  suite.type = 'TestSuite';
  suite.cases = cases;
  suite.count = length(cases);
  suite.created_at = discover_timestamp();
end
