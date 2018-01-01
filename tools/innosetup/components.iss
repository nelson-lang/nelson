;==============================================================================
; Copyright (c) 2016-2018 Allan CORNET (Nelson)
;==============================================================================
; LICENCE_BLOCK_BEGIN
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 2 of the License, or
; (at your option) any later version.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.
; LICENCE_BLOCK_END
;==============================================================================
#define COMPONENT_NELSON 'Nelson'
Name: {#COMPONENT_NELSON}; Description: Nelson {#APPLICATION_VERSION}; Types: full compact custom; Flags: fixed;
;
#define COMPONENT_CPU_OPTIMIZATION 'CPU_OPTIMIZATION'
#define COMPONENT_MKL_CPU_LIBRARY 'CPU_OPTIMIZATION\MKL'
#define COMPONENT_DEFAULT_CPU_LIBRARY 'CPU_OPTIMIZATION\DEFAULT'
;
Name: {#COMPONENT_CPU_OPTIMIZATION}; Description:{cm:CPU_OPTIMIZATION_FOR_NELSON}; Types: full compact custom; Flags: fixed;
Name: {#COMPONENT_MKL_CPU_LIBRARY}; Description:{cm:MKL_LIBRARIES}; Flags: exclusive
Name: {#COMPONENT_DEFAULT_CPU_LIBRARY}; Description:{cm:DEFAULT_LIBRARIES}; Flags: exclusive
;==============================================================================
