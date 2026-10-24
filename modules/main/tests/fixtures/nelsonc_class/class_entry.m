%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function result = class_entry()
  object = ncclasses.NcFactory.create(4);
  result = object.inheritedValue() + externalValue(object) + object.Offset;
  fprintf('CLASS_VALUE=%d\n', result);
end
