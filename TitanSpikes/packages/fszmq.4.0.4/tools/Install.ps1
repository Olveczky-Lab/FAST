# -------------------------------------------------------------------------
# This file is part of fszmq.
# 
# fszmq is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published 
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# fszmq is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Lesser General Public License for more details.
# 
# You should have received a copy of the GNU Lesser General Public License
# along with fszmq. If not, see <http://www.gnu.org/licenses/>.
# 
# Copyright (c) 2011-2013 Paulmichael Blasucci 
# -------------------------------------------------------------------------

param($installPath, $toolsPath, $package, $project)

# build path to appropriate version of native library
$libzmqPath = $toolsPath + "\zeromq\"
if ($project.ConfigurationManager.ActiveConfiguration.PlatformName -eq "x86")
{
  # use 32-bit native library
  $libzmqPath = $libzmqPath + "x86\libzmq.dll"
}
else
{
  # use 64-bit native library
  $libzmqPath = $libzmqPath + "x64\libzmq.dll"
}

$libzmqFile = $project.ProjectItems.AddFromFileCopy($libzmqPath)
if ($libzmqFile -ne $null)
{
  # BuildAction syntax varies by project type
  if ($project.Type -eq "F#")
  {
    $libzmqFile.Properties.Item("BuildAction").Value = 
      ([Microsoft.VisualStudio.FSharp.ProjectSystem.BuildAction]::None)
  }
  else
  {
    $libzmqFile.Properties.Item("BuildAction").Value = 0 # None
  }

  # CopyToOuptutDirectory is the same across project types
  $libzmqFile.Properties.Item("CopyToOutputDirectory").Value = 1 # CopyAlways
}
