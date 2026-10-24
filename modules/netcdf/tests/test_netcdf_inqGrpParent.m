%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--SEQUENTIAL TEST REQUIRED-->
asserts.isequal(nargin('netcdf.inqGrpParent'), -1);
asserts.isequal(nargout('netcdf.inqGrpParent'), -1);
ok = false; try; netcdf.inqGrpParent(); catch; ok = true; end; asserts.istrue(ok);
filename = [tempname(), '.nc'];
mode = bitor(netcdf.getConstant('NC_CLOBBER'), netcdf.getConstant('NC_NETCDF4'));
ncid = netcdf.create(filename, mode);
gid = netcdf.defGrp(ncid, 'g');
asserts.isequal(netcdf.inqNcid(ncid, 'g'), gid);
asserts.isequal(netcdf.inqGrpName(gid), 'g');
asserts.isequal(netcdf.inqGrpParent(gid), ncid);
asserts.istrue(length(netcdf.inqGrpNameFull(gid)) >= 2);
ids = netcdf.inqGrps(ncid);
asserts.istrue(any(ids == gid));
netcdf.close(ncid);
ok = false; try; netcdf.inqNcid(-1, 'g'); catch; ok = true; end; asserts.istrue(ok);
%=============================================================================



