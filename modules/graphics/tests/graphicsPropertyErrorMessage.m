%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function text = graphicsPropertyErrorMessage(propertyName, identifier, varargin)
  prefix = getString(message('Nelson:graphics:gotErrorProperty', propertyName));
  detail = getString(message(identifier, varargin{:}));
  text = [prefix, newline(), detail];
end
%=============================================================================
