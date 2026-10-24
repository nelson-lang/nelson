%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% meta.package.fromName returns a meta.package object for an existing package
p = meta.package.fromName('netcdf');
asserts.istrue(~isempty(p));
asserts.isequal(class(p), 'meta.package');
asserts.istrue(isa(p, 'meta.package'));
asserts.isequal(p.Name, 'netcdf');
asserts.istrue(iscell(p.FunctionList));
asserts.istrue(any(strcmp(p.FunctionList, 'netcdf.close')));
asserts.isequal(p.ContainingPackage, '');
%=============================================================================
% a nested package is reported through PackageList of its parent
np = meta.package.fromName('nelson');
asserts.istrue(~isempty(np));
asserts.isequal(np.Name, 'nelson');
asserts.istrue(iscell(np.PackageList));
asserts.istrue(any(strcmp(np.PackageList, 'nelson.lang')));
%=============================================================================
% the nested package resolves directly and knows its containing package
sp = meta.package.fromName('nelson.lang');
asserts.istrue(~isempty(sp));
asserts.isequal(sp.Name, 'nelson.lang');
asserts.isequal(sp.ContainingPackage, 'nelson');
%=============================================================================
% an unknown package name returns an empty 0x0 meta.package
q = meta.package.fromName('this_is_not_a_package_xyz');
asserts.istrue(isempty(q));
asserts.isequal(size(q), [0 0]);
asserts.isequal(class(q), 'meta.package');
%=============================================================================
% getAllPackages lists the available top-level packages
allp = meta.package.getAllPackages();
asserts.istrue(~isempty(allp));
asserts.isequal(class(allp), 'meta.package');
names = {allp.Name};
asserts.istrue(any(strcmp(names, 'netcdf')));
%=============================================================================
