%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
interp = tsdata.interpolation('zoh');
asserts.isequal(interp.Method, 'zoh');
asserts.isequal(interp == tsdata.interpolation('zoh'), true);
asserts.isequal(get(interp, 'Method'), 'zoh');
%=============================================================================
dataInfo = tsdata.datametadata('Units', 'm', 'Interpolation', 'nearest');
asserts.isequal(dataInfo.Units, 'm');
asserts.isequal(dataInfo.Interpolation.Method, 'nearest');
asserts.isequal(get(dataInfo, 'Units'), 'm');
dataInfo = set(dataInfo, 'Units', 'cm');
asserts.isequal(dataInfo.Units, 'cm');
dataInfo2 = tsdata.datametadata('Units', 'cm', 'Interpolation', 'nearest');
asserts.isequal((dataInfo + dataInfo2).Units, 'cm');
asserts.isequal((dataInfo + tsdata.datametadata('Units', 'm')).Units, '');
asserts.istrue(contains(evalc('display(dataInfo)'), 'datametadata'));
%=============================================================================
timeInfo = tsdata.timemetadata('Units', 'seconds', 'StartDate', '01-Jan-2024');
timeInfo = setTime(timeInfo, [0; 1; 2]);
asserts.isequal(timeInfo.Length, 3);
asserts.isequal(timeInfo.Start, 0);
asserts.isequal(timeInfo.End, 2);
asserts.isequal(timeInfo.Increment, 1);
asserts.isequal(getData(timeInfo), [0; 1; 2]);
asserts.isequal(get(timeInfo, 'Units'), 'seconds');
timeInfo = set(timeInfo, 'Format', 'HH:mm:ss');
asserts.isequal(timeInfo.Format, 'HH:mm:ss');
asserts.isequal(getTimeStr(timeInfo), '01-Jan-2024');
otherTimeInfo = setTime(tsdata.timemetadata(), [3; 4]);
mergedTimeInfo = timemerge(timeInfo, otherTimeInfo);
asserts.isequal(mergedTimeInfo.Length, 5);
asserts.istrue(contains(evalc('display(timeInfo)'), 'timemetadata'));
timeInfo = reset(timeInfo);
asserts.isequal(timeInfo.Length, 0);
%=============================================================================
qualityInfo = tsdata.qualmetadata('Code', [0 1], 'Description', {'ok', 'bad'});
asserts.isequal(qualityInfo.Code, [0 1]);
asserts.isequal(qualityInfo.Description, {'ok'; 'bad'});
asserts.isequal(get(qualityInfo, 'Code'), [0 1]);
qualityInfo = set(qualityInfo, 'UserData', 42);
asserts.isequal(qualityInfo.UserData, 42);
asserts.istrue(contains(evalc('display(qualityInfo)'), 'qualmetadata'));
%=============================================================================
ev = tsdata.event('start', 10, 'Units', 'seconds');
asserts.isequal(ev.Name, 'start');
asserts.isequal(ev.Time, 10);
asserts.isequal(get(ev, 'Units'), 'seconds');
ev = set(ev, 'UserData', 7);
asserts.isequal(ev.UserData, 7);
asserts.isequal(ev == tsdata.event('start', 10, 'Units', 'seconds'), true);
asserts.isequal(getTimeStr(ev), '10 seconds');
[found, index] = findEvent([tsdata.event('other', 1), ev], 'start');
asserts.isequal(index, 2);
asserts.isequal(found.Name, 'start');
asserts.istrue(contains(evalc('display(ev)'), 'event'));
%=============================================================================
