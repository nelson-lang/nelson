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
filewrite([testdir, '/NelsonHandleSharedState.m'], [
  "classdef NelsonHandleSharedState < handle";
  "  properties";
  "    Value = 0";
  "  end";
  "end"]);
filewrite([testdir, '/makeNelsonHandleSharedState.m'], [
  "function h = makeNelsonHandleSharedState()";
  "h = NelsonHandleSharedState();";
  "end"]);
addpath(testdir);
%=============================================================================
a = NelsonHandleSharedState();
b = a;
clear a;
asserts.istrue(isvalid(b));
%=============================================================================
delete(b);
asserts.isfalse(isvalid(b));
delete(b);
asserts.isfalse(isvalid(b));
%=============================================================================
c = NelsonHandleSharedState();
handles = [c, c];
items = {c};
holder.item = c;
clear c;
asserts.isequal(isvalid(handles), [true true]);
asserts.istrue(isvalid(items{1}));
asserts.istrue(isvalid(holder.item));
delete(items{1});
asserts.isequal(isvalid(handles), [false false]);
asserts.isfalse(isvalid(holder.item));
%=============================================================================
returned = makeNelsonHandleSharedState();
asserts.istrue(isvalid(returned));
delete(returned);
asserts.isfalse(isvalid(returned));
%=============================================================================
rmpath(testdir);
rmdir(testdir, 's');
%=============================================================================
