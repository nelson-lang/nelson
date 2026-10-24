%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Installing a local archive path that does not exist must say so instead of
% falling back to a registry lookup (which needs webtools and reports that).
%=============================================================================
missing = [tempdir(), 'does_not_exist-all-1.0.0.nmz'];
if isfile(missing)
  rmfile(missing);
end
asserts.checkerror('nmm(''install'', missing)', sprintf('Package archive not found: %s', missing));
%=============================================================================
