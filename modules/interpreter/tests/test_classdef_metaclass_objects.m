%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
addpath([modulepath('interpreter', 'tests'), '/classdef']);
%=============================================================================
% metaclass returns nelson.metadata.Class objects
mc = metaclass(ClassdefPoint(1, 2));
asserts.isequal(class(mc), 'nelson.metadata.Class');
% the legacy meta.class name stays a recognized superclass alias
asserts.istrue(isa(mc, 'nelson.metadata.Class'));
asserts.istrue(isa(mc, 'meta.class'));
asserts.isequal(mc.Name, 'ClassdefPoint');
asserts.isfalse(mc.Abstract);
asserts.isfalse(mc.Sealed);
asserts.isfalse(mc.Handle);
%=============================================================================
% property, method, and enumeration lists are typed object arrays
asserts.isequal(class(mc.PropertyList), 'nelson.metadata.Property');
names = {mc.PropertyList.Name};
asserts.istrue(any(strcmp(names, 'X')));
asserts.istrue(any(strcmp(names, 'Dimension')));
xProperty = mc.PropertyList(find(strcmp(names, 'X')));
asserts.isequal(xProperty.DefiningClass, 'ClassdefPoint');
asserts.isfalse(xProperty.Constant);
dimensionProperty = mc.PropertyList(find(strcmp(names, 'Dimension')));
asserts.istrue(dimensionProperty.Constant);
asserts.isequal(class(mc.MethodList), 'nelson.metadata.Method');
methodNames = {mc.MethodList.Name};
asserts.istrue(any(strcmp(methodNames, 'magnitude')));
%=============================================================================
% superclass list is a nelson.metadata.Class array
cc = metaclass('ClassdefCounter');
asserts.isequal(class(cc.SuperclassList), 'nelson.metadata.Class');
asserts.istrue(any(strcmp({cc.SuperclassList.Name}, 'handle')));
asserts.istrue(cc.Handle);
asserts.isequal(class(cc.EventList), 'nelson.metadata.Event');
asserts.istrue(any(strcmp({cc.EventList.Name}, 'CountChanged')));
%=============================================================================
% ?Class syntax returns the same metadata objects
qc = ?ClassdefPoint;
asserts.isequal(class(qc), 'nelson.metadata.Class');
asserts.isequal(qc.Name, 'ClassdefPoint');
%=============================================================================
% enumeration members
ec = metaclass('ClassdefColor');
asserts.isequal(class(ec.EnumerationMemberList), 'nelson.metadata.EnumeratedValue');
asserts.istrue(any(strcmp({ec.EnumerationMemberList.Name}, 'Red')));
%=============================================================================
% Nelson metadata structures remain available for compatibility
asserts.istrue(isstruct(mc.ClassDetails));
asserts.istrue(isstruct(mc.PropertyDetails));
asserts.istrue(any(strcmp({mc.PropertyDetails.Name}, 'X')));
%=============================================================================
% superclasses returns the ancestor names, nearest first, as a cell of char
sc = superclasses('ClassdefAbstractMethodImpl');
asserts.istrue(iscell(sc));
asserts.isequal(sc, {'ClassdefAbstractMethodBase'});
scObj = superclasses(ClassdefPoint(0, 0));
asserts.istrue(iscell(scObj));
%=============================================================================
