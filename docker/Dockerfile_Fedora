#==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
#==============================================================================
# This file is part of the Nelson.
#=============================================================================
# LICENCE_BLOCK_BEGIN
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
#
# Alternatively, you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 2 of
# the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this program. If not, see <http://www.gnu.org/licenses/>.
# LICENCE_BLOCK_END
#==============================================================================

FROM fedora:30
MAINTAINER Allan CORNET "nelson.numerical.computation@gmail.com"

ARG BRANCH_NAME
RUN echo "Nelson's branch: ${BRANCH_NAME}"

RUN dnf update -y
RUN dnf upgrade -y
RUN dnf install -y which
RUN dnf install -y hostname
RUN dnf install -y git
RUN dnf install -y make
RUN dnf install -y libtool
RUN dnf install -y gcc
RUN dnf install -y gcc-c++ 
RUN dnf install -y autoconf 
RUN dnf install -y automake
RUN dnf install -y openmpi-devel
RUN dnf install -y gettext
RUN dnf install -y pkg-config
RUN dnf install -y cmake
RUN dnf install -y libffi-devel
RUN dnf install -y libicu-devel
RUN dnf install -y libxml2-devel
RUN dnf install -y lapack-devel
RUN dnf install -y fftw3-devel
RUN dnf install -y portaudio-devel.x86_64
RUN dnf install -y libsndfile-devel.x86_64
RUN dnf install -y taglib-devel
RUN dnf install -y qt5-devel
RUN dnf install -y qt5-assistant
RUN dnf install -y qt5-doctools
RUN dnf install -y qt5-qtquickcontrols
RUN dnf install -y boost-devel

RUN git clone https://github.com/live-clones/hdf5.git /tmp/hdf5_1_10_5 
RUN cd /tmp/hdf5_1_10_5 && git checkout hdf5-1_10_5 && ./configure --quiet --enable-shared --disable-deprecated-symbols --disable-hl --disable-strict-format-checks --disable-memory-alloc-sanity-check --disable-instrument --disable-parallel --disable-trace --disable-asserts --with-pic --with-default-api-version=v110 CFLAGS="-w" && make install -C src

RUN git clone https://github.com/tbeu/matio /tmp/matio 
RUN cd /tmp/matio && git checkout v1.5.15 && cd /tmp/matio && ./autogen.sh && ./configure --enable-shared --enable-mat73=yes --enable-extended-sparse=no --with-pic --with-hdf5=/tmp/hdf5_1_10_5/hdf5 && make && make install;

RUN git clone https://github.com/eigenteam/eigen-git-mirror /tmp/eigen
RUN mkdir /tmp/eigen-build && cd /tmp/eigen && git checkout 3.3.7 && cd - && cd /tmp/eigen-build && cmake . /tmp/eigen && make -j4 && make install;

RUN git clone https://github.com/Nelson-numerical-software/nelson.git
WORKDIR "/nelson"
RUN cd "/nelson" && git checkout ${BRANCH_NAME}

RUN mkdir /home/nelsonuser

RUN groupadd -g 1001 nelsonuser && \
    useradd -r -u 1001 -g nelsonuser nelsonuser

RUN chown -R nelsonuser:nelsonuser /home/nelsonuser

RUN chown -R nelsonuser:nelsonuser /nelson

USER nelsonuser

ENV AUDIODEV null
ENV PATH="/usr/lib64/openmpi/bin:${PATH}"
ENV HDF5_LIBRARIES="/tmp/hdf5_1_10_5/hdf5/lib"
ENV HDF5_INCLUDE_DIRS="/tmp/hdf5_1_10_5/hdf5"
ENV HDF5_ROOT="/tmp/hdf5_1_10_5/hdf5"
ENV LD_LIBRARY_PATH="${HDF5_LIBRARIES}:/usr/lib64/openmpi/lib:${LD_LIBRARY_PATH}"
RUN cmake -G "Unix Makefiles" .
RUN make -j4

RUN make buildhelp
RUN make tests_minimal
RUN make package
RUN make tests_all_no_display
    

ENTRYPOINT ["/nelson/bin/linux64/nelson-cli"]
