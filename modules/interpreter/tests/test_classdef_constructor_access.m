%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
addpath([modulepath('interpreter', 'tests'), '/classdef']);
%=============================================================================
% protected constructor: direct call blocked, static factory allowed
asserts.checkerror('ClassdefProtectedCtor();', ...
  _('Cannot access method ''ClassdefProtectedCtor'' in class ''ClassdefProtectedCtor''.'));
o = ClassdefProtectedCtor.create('t');
asserts.isequal(o.Tag, 't');
%=============================================================================
% protected constructor: callable from a subclass constructor
c = ClassdefProtectedCtorChild('u');
asserts.isequal(c.Tag, 'u');
%=============================================================================
% private constructor: direct call and subclass call blocked, factory allowed
asserts.checkerror('ClassdefPrivateCtor();', ...
  _('Cannot access method ''ClassdefPrivateCtor'' in class ''ClassdefPrivateCtor''.'));
asserts.checkerror('ClassdefPrivateCtorChild();', ...
  _('Cannot access method ''ClassdefPrivateCtor'' in class ''ClassdefPrivateCtor''.'));
p = ClassdefPrivateCtor.create('v');
asserts.isequal(p.Tag, 'v');
%=============================================================================
% friend class list on the constructor
asserts.checkerror('ClassdefFriendCtor();', ...
  _('Cannot access method ''ClassdefFriendCtor'' in class ''ClassdefFriendCtor''.'));
f = ClassdefFriendCtorFactory.build();
asserts.isequal(f.Tag, 'friend');
%=============================================================================
