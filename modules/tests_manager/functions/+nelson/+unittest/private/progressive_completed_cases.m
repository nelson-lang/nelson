%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function testCases = progressive_completed_cases(state)
  completedCount = state.cursor - 1;
  if completedCount == 0
    testCases = [];
    return
  end
  completed = state.results(1:completedCount);
  try
    testCases = repmat(completed{1}, completedCount, 1);
    for k = 2:completedCount
      testCases(k) = completed{k};
    end
    return
  catch
  end
  fieldTemplate = completed{1};
  for k = 2:completedCount
    [fieldTemplate, completed{k}] = ...
      align_test_case_fields(fieldTemplate, completed{k});
  end
  templateNames = fieldnames(fieldTemplate);
  for k = 1:completedCount
    for fieldIndex = 1:length(templateNames)
      fieldName = templateNames{fieldIndex};
      if ~isfield(completed{k}, fieldName)
        completed{k}.(fieldName) = [];
      end
    end
    completed{k} = orderfields(completed{k}, templateNames);
  end
  testCases = repmat(completed{1}, completedCount, 1);
  for k = 2:completedCount
    testCases(k) = completed{k};
  end
end
%=============================================================================
