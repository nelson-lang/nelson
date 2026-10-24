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
filewrite([testdir, 'WeakHandleNode.m'], [
  "classdef WeakHandleNode < handle";
  "  properties";
  "    Children = WeakHandleNode.empty";
  "    Name = ''";
  "  end";
  "  properties (WeakHandle)";
  "    Parent WeakHandleNode";
  "    Peers (1,:) WeakHandleNode";
  "  end";
  "  methods";
  "    function obj = WeakHandleNode(name)";
  "      if nargin > 0";
  "        obj.Name = name;";
  "      end";
  "    end";
  "    function add(obj, child)";
  "      obj.Children(end + 1) = child;";
  "      child.Parent = obj;";
  "    end";
  "    function delete(obj)";
  "      global WEAK_HANDLE_DELETED";
  "      WEAK_HANDLE_DELETED{end + 1} = obj.Name;";
  "    end";
  "  end";
  "end"]);
filewrite([testdir, 'WeakHandleCopyable.m'], [
  "classdef WeakHandleCopyable < nelson.mixin.Copyable";
  "  properties (WeakHandle)";
  "    Target WeakHandleNode";
  "  end";
  "end"]);
filewrite([testdir, 'WeakHandleNoValidation.m'], [
  "classdef WeakHandleNoValidation < handle";
  "  properties (WeakHandle)";
  "    Target";
  "  end";
  "end"]);
filewrite([testdir, 'WeakHandleValueClass.m'], [
  "classdef WeakHandleValueClass";
  "  properties (WeakHandle)";
  "    Target WeakHandleNode";
  "  end";
  "end"]);
filewrite([testdir, 'WeakHandleConstant.m'], [
  "classdef WeakHandleConstant < handle";
  "  properties (WeakHandle, Constant)";
  "    Target WeakHandleNode = WeakHandleNode.empty";
  "  end";
  "end"]);
addpath(testdir);
global WEAK_HANDLE_DELETED
WEAK_HANDLE_DELETED = {};
%=============================================================================
% default value: deleted handles of the validated class, no object constructed
node = WeakHandleNode('node');
asserts.isequal(class(node.Parent), 'WeakHandleNode');
asserts.isequal(size(node.Parent), [1 1]);
asserts.isfalse(isvalid(node.Parent));
asserts.isequal(class(node.Peers), 'WeakHandleNode');
asserts.isequal(size(node.Peers), [1 0]);
asserts.isequal(WEAK_HANDLE_DELETED, {});
%=============================================================================
% the weak reference returns the same object while it is alive
parent = WeakHandleNode('parent');
node.Parent = parent;
asserts.istrue(isvalid(node.Parent));
asserts.istrue(node.Parent == parent);
asserts.isequal(node.Parent.Name, 'parent');
%=============================================================================
% the weak reference does not keep its target alive
clear parent
asserts.isequal(WEAK_HANDLE_DELETED, {'parent'});
asserts.isfalse(isvalid(node.Parent));
asserts.isequal(class(node.Parent), 'WeakHandleNode');
%=============================================================================
% explicit delete of the target
other = WeakHandleNode('other');
node.Parent = other;
delete(other);
asserts.isfalse(isvalid(node.Parent));
WEAK_HANDLE_DELETED = {};
%=============================================================================
% parent/child tree without a strong cycle: clearing the root frees everything
root = WeakHandleNode('root');
child = WeakHandleNode('child');
root.add(child);
asserts.istrue(child.Parent == root);
clear child
asserts.isequal(WEAK_HANDLE_DELETED, {});
clear root
asserts.isequal(WEAK_HANDLE_DELETED, {'root', 'child'});
WEAK_HANDLE_DELETED = {};
%=============================================================================
% arrays of weak handles: each element is released on its own
a = WeakHandleNode('a');
b = WeakHandleNode('b');
node.Peers = [a, b];
asserts.isequal(size(node.Peers), [1 2]);
clear a
peers = node.Peers;
asserts.isequal(isvalid(peers), [false, true]);
asserts.istrue(peers(2) == b);
clear peers
asserts.isequal(WEAK_HANDLE_DELETED, {'a'});
WEAK_HANDLE_DELETED = {};
%=============================================================================
% copy keeps the weak reference weak
target = WeakHandleNode('target');
source = WeakHandleCopyable();
source.Target = target;
duplicate = copy(source);
asserts.istrue(duplicate.Target == target);
clear target
asserts.isfalse(isvalid(source.Target));
asserts.isfalse(isvalid(duplicate.Target));
WEAK_HANDLE_DELETED = {};
%=============================================================================
m = metaclass(node);
names = {m.PropertyList.Name};
asserts.isequal(m.PropertyList(strcmp(names, 'Parent')).Attributes, {'WeakHandle'});
%=============================================================================
asserts.checkerror('WeakHandleNoValidation();', ...
    'WeakHandle properties must use class validation: Target', ...
    'Nelson:class:weakHandleRequiresClassValidation');
asserts.checkerror('WeakHandleValueClass();', ...
    'WeakHandle properties are only allowed in handle classes: Target', ...
    'Nelson:class:weakHandleRequiresHandleClass');
asserts.checkerror('WeakHandleConstant();', ...
    'Invalid classdef attribute combination: WeakHandle and Constant or Dependent property: Target', ...
    'Nelson:interpreter:invalidAttributeCombination');
%=============================================================================
clear node
clear global WEAK_HANDLE_DELETED
rmpath(testdir);
rmdir(testdir, 's');
%=============================================================================
