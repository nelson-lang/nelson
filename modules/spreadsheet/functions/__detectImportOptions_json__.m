%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function opts = __detectImportOptions_json__(filename, varargin)
  % JSON backend of detectImportOptions (called by the builtin).
  args = jsonTableArgs(varargin);
  [root, rootIsArray, columns] = jsonTableDecode(convertStringsToChars(filename), args.Options, ...
    jsonTableColumnar(args.Options, args.TableNodeName, any(strcmp(args.Set, 'TableSelector'))));
  opts = jsonTableDetect(root, rootIsArray, args, columns);
end
%=============================================================================
