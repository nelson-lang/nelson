%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
base = MException('Nelson:test:base', 'Base message.');
cause = MException('Nelson:test:cause', 'Cause message.');
%=============================================================================
withCause = addCause(base, cause);
asserts.isequal(class(withCause), 'MException');
asserts.isequal(length(withCause.cause), 1);
asserts.isequal(withCause.cause{1}.identifier, 'Nelson:test:cause');
asserts.isequal(withCause.cause{1}.message, 'Cause message.');
asserts.isequal(base.cause, {});
%=============================================================================
withCause = base.addCause(cause);
asserts.isequal(length(withCause.cause), 1);
asserts.isequal(withCause.cause{1}.identifier, 'Nelson:test:cause');
%=============================================================================
