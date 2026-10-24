%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% warning('off', 'all') suppresses the display of warnings that have no
% individual state, but they are still recorded by lastwarn.
warning('off', 'all');
lastwarn('');
warning('MyId:test', 'suppressed but recorded');
[msg, id] = lastwarn();
asserts.isequal(id, 'MyId:test');
asserts.isequal(msg, 'suppressed but recorded');
warning('on', 'all');
%=============================================================================
% A warning with an individual 'off' state is also recorded by lastwarn.
warning('off', 'Some:id');
lastwarn('');
warning('Some:id', 'also recorded');
[msg2, id2] = lastwarn();
asserts.isequal(id2, 'Some:id');
warning('on', 'Some:id');
%=============================================================================
