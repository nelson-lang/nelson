%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Pure-Nelson compatibility shim for the common idiom
%   char(java.util.UUID.randomUUID)
% used to generate a unique identifier without relying on a Java runtime.
% Returns a random (version 4) UUID as a lowercase character vector in the
% canonical 8-4-4-4-12 hexadecimal form, e.g. '3f2504e0-4f89-41d3-9a0c-0305e82c3301'.
% Delegates to the native createGUID builtin, which generates the same
% RFC 4122 version-4 form without disturbing the global random number stream.
function uuid = randomUUID()
  uuid = createGUID();
end
%=============================================================================
