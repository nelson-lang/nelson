%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function T = __readtable_json__(filename, varargin)
  % JSON backend of readtable and readtimetable (called by the builtins).
  if ~isempty(varargin) && isa(varargin{1}, 'nelson.io.json.JSONImportOptions')
    if numel(varargin) > 1
      error('Nelson:spreadsheet:importOptionsExpected', 'Import options object expected.');
    end
    opts = varargin{1};
    [root, rootIsArray, columns] = jsonTableDecode(convertStringsToChars(filename), opts, ...
      jsonTableColumnar(opts, '', true));
    T = jsonTableRead(root, opts, jsonTableCache(root, rootIsArray, opts.TableSelector, columns));
    return
  end
  args = jsonTableArgs(varargin);
  [root, rootIsArray, columns] = jsonTableDecode(convertStringsToChars(filename), args.Options, ...
    jsonTableColumnar(args.Options, args.TableNodeName, any(strcmp(args.Set, 'TableSelector'))));
  [opts, cache] = jsonTableDetect(root, rootIsArray, args, columns);
  T = jsonTableRead(root, opts, cache);
end
%=============================================================================
