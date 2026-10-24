%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ReplaceIdentifierCorrection
  properties (SetAccess = private)
    Identifier
    Replacement
  end
  methods
    function obj = ReplaceIdentifierCorrection(identifier, replacement)
      obj.Identifier = identifier;
      obj.Replacement = replacement;
    end
  end
end
%=============================================================================
