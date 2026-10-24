%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
testdir = [tempname(), '/'];
mkdir(testdir);
filewrite([testdir, '/NelsonWeakReferenceTarget.m'], [
  "classdef NelsonWeakReferenceTarget < handle";
  "  properties";
  "    Value = 0";
  "  end";
  "end"]);
addpath(testdir);
%=============================================================================
placeholder = nelson.lang.HandlePlaceholder();
asserts.istrue(isa(placeholder, 'handle'));
asserts.istrue(isvalid(placeholder));
delete(placeholder);
asserts.isfalse(isvalid(placeholder));
%=============================================================================
missing = nelson.lang.invalidHandle('nelson.lang.HandlePlaceholder');
asserts.isequal(class(missing), 'nelson.lang.HandlePlaceholder');
asserts.isfalse(isvalid(missing));
missingArray = nelson.lang.invalidHandle('nelson.lang.HandlePlaceholder', 2, 3);
asserts.isequal(size(missingArray), [2 3]);
asserts.isequal(isvalid(missingArray), false(2, 3));
asserts.checkerror('nelson.lang.invalidHandle(''double'');', ...
    _('Handle class not found: double'));
%=============================================================================
weak = nelson.lang.WeakReference();
asserts.isequal(class(weak.Handle), 'nelson.lang.HandlePlaceholder');
asserts.isfalse(isvalid(weak.Handle));
asserts.checkerror('weak.ValidHandle;', _('Weak reference target is an invalid handle.'));
%=============================================================================
target = NelsonWeakReferenceTarget();
target.Value = 42;
weak = nelson.lang.WeakReference(target);
asserts.istrue(isvalid(weak.Handle));
asserts.isequal(weak.Handle.Value, 42);
strong = weak.Handle;
clear target;
asserts.istrue(isvalid(weak.Handle));
asserts.isequal(strong.Value, 42);
clear strong;
asserts.isfalse(isvalid(weak.Handle));
asserts.isequal(class(weak.Handle), 'NelsonWeakReferenceTarget');
asserts.checkerror('weak.ValidHandle;', _('Weak reference target is an invalid handle.'));
%=============================================================================
target = NelsonWeakReferenceTarget();
weak.Handle = target;
asserts.istrue(isvalid(weak.ValidHandle));
delete(target);
asserts.isfalse(isvalid(weak.Handle));
asserts.isequal(class(weak.Handle), 'NelsonWeakReferenceTarget');
%=============================================================================
rmpath(testdir);
rmdir(testdir, 's');
%=============================================================================
