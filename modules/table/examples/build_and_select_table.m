%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Build a table and select rows using a logical condition.
city = {'Paris'; 'Lyon'; 'Lille'; 'Nice'};
temperature = [21.2; 24.8; 19.7; 26.1];
rain = [false; false; true; false];
weather = table(city, temperature, rain, 'VariableNames', {'City', 'Temperature', 'Rain'});
warmAndDry = weather(weather.Temperature > 22 & ~weather.Rain, :);
disp(warmAndDry);
