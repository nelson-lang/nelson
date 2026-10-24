%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function cache = jsonTableCache(root, rootIsArray, selector, columns)
  % Rows of the table node selected by 'selector': the decoded rows
  % (Rows, Keys, see jsonTableRows) or, when 'columns' is not empty, the
  % columns decoded by __jsondecode_columns__ (Columnar).
  % Columns and Vectors receive the variables extracted by jsonTableDetect.
  cache.Columnar = columns;
  cache.Rows = [];
  cache.Keys = {};
  cache.Columns = {};
  cache.Vectors = {};
  if ~isempty(columns)
    cache.Count = numel(columns.Columns{1});
    return
  end
  [node, found] = jsonPointerGet(root, selector);
  if ~found
    error('Nelson:spreadsheet:jsonPointerNotFound', 'JSON Pointer not found: %s', selector);
  end
  [cache.Rows, cache.Keys] = jsonTableRows(node, isempty(selector) && rootIsArray);
  cache.Count = numel(cache.Rows);
end
%=============================================================================
