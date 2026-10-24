%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Regression: resolving a method call such as R.max(...) routes through
% ismethod, which enumerated the object's method/property names. That
% enumeration held the unrooted propertynames() array (and the bare element
% pointers read out of it) while calling getproperty()/isa() for each entry.
% Those calls allocate, so on a module like Base -- whose propertynames() list
% is huge -- a collection would eventually fire mid-loop and free the array,
% leaving a dangling pointer and a SIGSEGV after ~150-500 repeated calls.
% The name-enumeration and type helpers now hold the collector off for the
% duration. Hammer the dispatch path well past the old crash window.
%=============================================================================
juliaRequireEngineForTest();
%=============================================================================
R = jlrun('A = Base', 'A');
%=============================================================================
% ismethod/isprop drive the same enumeration that used to crash.
asserts.istrue(ismethod(R, 'max'));
asserts.istrue(ismethod(R, 'hypot'));
asserts.istrue(isprop(R, 'pi'));
%=============================================================================
for k = 1:800
  asserts.isequal(R.max(1.0, 2.0), 2);
  asserts.isapprox(R.hypot(3.0, 4.0), 5, 1e-9);
  asserts.isequal(R.min(7.0, 4.0), 4);
end
%=============================================================================
