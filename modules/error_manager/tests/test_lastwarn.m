%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('lastwarn'), 2);
asserts.isequal(nargout('lastwarn'), 2);
%=============================================================================
lastwarn();
%=============================================================================
msg = lastwarn();
asserts.isequal(msg, '');
%=============================================================================
[msg, id] = lastwarn();
asserts.isequal(msg, '');
asserts.isequal(id, '');
%=============================================================================
lastwarn('toto');
[msg, id] = lastwarn();
asserts.isequal(msg, 'toto');
asserts.isequal(id, '');
%=============================================================================
lastwarn('');
[msg, id] = lastwarn();
asserts.isequal(msg, '');
asserts.isequal(id, '');
%=============================================================================
lastwarn('toto', 'titi');
[msg, id] = lastwarn();
asserts.isequal(msg, 'toto');
asserts.isequal(id, 'titi');
%=============================================================================
lastwarn('');
[msg, id] = lastwarn();
asserts.isequal(msg, '');
asserts.isequal(id, '');
%=============================================================================
