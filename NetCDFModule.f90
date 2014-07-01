MODULE NetCDFModule
	
	!==================================================================!
	!
	! Module: NetCDF Module 
	! Author: Chris Chapman (ANU) 
	! Date: 12st Sep 2010
	! Uses: netcdf 
	!
	!
	! Description: This module defines the NetCDF file object.  It 
	! contains routines for writing and reading data from netcdf files
	! as well as creation and interrogation
	!
	! Types	  : netcdfFile
	! Contains: Constructors and Destructors 
	!		    WriteDataToFile
	!			WriteDimToFile
	!
	!==================================================================!

	
	
	use netcdf
	implicit none
	
	PRIVATE CheckForNetCDFError, CheckFileExistance, CheckIfFileOpen
	PRIVATE CheckVarType
	
	PRIVATE ChangeDataToDefineMode, ChangeDefineToDataMode
	
	PUBLIC  New, Delete
	PUBLIC  WriteDataToFile, WriteDimToFile
	PUBLIC  CreateNewVariable
	
	integer, PRIVATE					:: netcdfStatus
	
	!==================================================================!
	! Define the netcdf file ADT 
	!==================================================================!
	
	type netcdfFile
		private
		
		
		character(len=256)				::	fileName					 		!Name of the net cdf file
		integer							::  fileID								!File ID for use in the API
		
		!Vectors that hold the names of all dimensions and nvariables created 
		character(len=16)				:: 	dimensionNames(NF90_MAX_VAR_DIMS)	!
		integer							::	dimensionIDs(NF90_MAX_VAR_DIMS)
		integer							:: 	numberOfDimensions	= 0
		
		character(len=16)				::	variableNames(NF90_MAX_VAR_DIMS)
		integer							:: 	variableIDs(NF90_MAX_VAR_DIMS)
		integer							::	numberOfVariables 	= 0 
		
		!!Flags
		
		logical							::	fileIsOpen 		= .false.
		logical							:: 	fileExists 		= .false.
		logical							:: 	fileIsInDefMode = .false.
		
	end type netcdfFile

	!==================================================================!
	! Standard Object Methods: Construction, Destruction
	!==================================================================!

	interface New
		module procedure NewFilePrivate
	end interface New

	interface Delete
		module procedure DeleteFilePrivate
	end interface Delete

	!==================================================================!
	! NetCDF file I/O interfaces
	!==================================================================!
	
	interface WriteDataToFile
		module procedure WriteToFile1D_Dble
		module procedure WriteToFile1D_Dble_SingleRecord
		module procedure WriteToFile2D_Dble
		module procedure WriteToFile2D_Dble_SingleRecord

		module procedure WriteToFile3D_Dble
		module procedure WriteToFile3D_Dble_SingleRecord
		
		module procedure WriteToFile2D_Int

	end interface WriteDataToFile 
	
	interface WriteDimToFile
		module procedure CreateDimensionPrivate
		module procedure CreateUnlimitedDimension
	end interface 
	
	!==================================================================!
	! Variable creation
	!==================================================================!
	
	interface CreateNewVariable
		module procedure CreateVariablePrivate
		module procedure CreateSingleDimensionVariable
	end interface CreateNewVariable
	

contains
!==================================================================!
! Standard Object Methods: Construction, Destruction
!==================================================================!


FUNCTION NewFilePrivate(inputFileName) result(self)
	
	!==================================================================!
	! Function: NewFilePrivate (Constructor)
	! Interface: New 
	!
	! Input arguments: inputFileName - the name of the file to 
	!								 associate with the object
	!
	! Output arguments: self - the instantiated netcdfFile object
	!
	! Description: Function creates a new netcdf file type.  This 
	! may be a new or existing netcdf file.  If it is a new file 
	! the function will create it and open it. If it is an existing 
	! file, the function will open it for I/0/.
	!
	!
	! Uses: netcdf API
	!==================================================================!
	
	!==================================================================!
	! Subroutine Input/Output Variables
	!==================================================================!
	
	character (len = *), intent(in) 				::	inputFileName
	type(netcdfFile)								:: 	self
	
	!==================================================================!
	! Internal Subroutine Variables
	!==================================================================!
	
	!Set the fileName of the derived type
	self%fileName = inputFileName
	
	!Does the file exist? 
	inquire(FILE=self%fileName, EXIST=self%fileExists)  				 ! fileExists will be TRUE if the file exists
	
	print *, 'file'
	if (self%fileExists)	then 
		
		!If the file exists, open it	
		print *, 'Using Existing File: ', self%fileName
		netcdfStatus = nf90_open(self%fileName, NF90_WRITE, self%fileID)
		
	else
		
		!If the file doesnot exist, create it
		print *, 'Creating New File: ', self%fileName
		netcdfStatus = nf90_create(self%fileName, NF90_CLOBBER, self%fileID)
		self%fileExists = .true. 
		
		!Take file out of define mode
		
		netcdfStatus = nf90_enddef(self%fileID)
		call CheckForNetCDFError(netcdfStatus)
		self%fileIsInDefMode = .false.
		
	end if																
	
	call CheckForNetCDFError(netcdfStatus)
	self%fileIsOpen=.true.
	
	
end FUNCTION NewFilePrivate

SUBROUTINE DeleteFilePrivate(self)

	type(netcdfFile), intent(inout)					:: self
	!integer											:: netcdfStatus
	
	netcdfStatus = nf90_close(self%fileID)
	call CheckForNetCDFError(netcdfStatus)

end SUBROUTINE DeleteFilePrivate

!==================================================================!
! NetCDF file I/O interfaces
!==================================================================!

SUBROUTINE  WriteToFile1D_Dble(self, arrayToWrite1D, variableName)

	type(netcdfFile), intent(inout)					::	self
	double precision, intent(	in)					::	arrayToWrite1D(:)
	character (len = *), intent(in) 		    	::	variableName
	
	integer											:: variableID	
	integer											:: iDimension 
	
	call CheckFileExistance(self%fileName)
	call CheckIfFileOpen(self) 
	
	
	!Check for the existance of the variable 
	netcdfStatus = nf90_inq_varid(self%fileID, variableName, variableID)
	call CheckForNetCDFError(netcdfStatus)

	
	if (CheckVarType(self, NF90_DOUBLE, variableID)) then
		
		print *, 'WARNING: Variables not of the same type'
		print *, 'Variable Name: ', variableName
		
	end if
	
	!Write the variable to file
	!Make sure that the file is in data mode
	call ChangeDefineToDataMode(self)
	
	netcdfStatus =  nf90_put_var(self%fileID, variableID, arrayToWrite1D)
	call CheckForNetCDFError(netcdfStatus)
	
		

end SUBROUTINE WriteToFile1D_Dble


SUBROUTINE WriteToFile1D_Dble_SingleRecord(self, arrayToWrite, variableName, timeStep)

	type(netcdfFile), intent(inout)					::	self 
	double precision, intent(	in)					::	arrayToWrite(:)
	character (len = *), intent(in) 		    	::	variableName
	integer,		     intent(in)					::	timeStep
	
	integer											::	variableID
	
	integer											::	countArray(2)
	integer											::	startArray(2)
	integer											::	iDim
	
	
	!Check for file existance
	call CheckFileExistance(self%fileName)
	
	!Make sure that the file is open
	call CheckIfFileOpen(self)
	
		
	startArray(1) = 1
	countArray(1) = size(arrayToWrite,1)
	
	
	startArray(2) = timeStep
	!print *, 'Time Step: ', timeStep
	countArray(2) = 1
	
	! Check if the variable you're trying to write already exists
	
	netcdfStatus = nf90_inq_varid(self%fileID, variableName, variableID)
	
	call CheckForNetCDFError(netcdfStatus)
	
	if (CheckVarType(self, NF90_DOUBLE, variableID)) then
		
		print *, 'WARNING: Variables not of the same type'
		print *, 'Variable Name: ', variableName
	
	end if
	
	
	!Write the variable to file
	!Make sure that the file is in data mode
	call ChangeDefineToDataMode(self)
	
	netcdfStatus =  nf90_put_var(self%fileID, variableID, arrayToWrite, start = startArray, count = countArray)
	call CheckForNetCDFError(netcdfStatus)

end SUBROUTINE WriteToFile1D_Dble_SingleRecord





SUBROUTINE WriteToFile2D_Int(self, arrayToWrite2D, variableName) 
	
	type(netcdfFile), intent(inout)					::	self 
	integer,		  intent(	in)					::	arrayToWrite2D(:,:)
	character (len = *), intent(in) 		    	::	variableName
	
	integer											::	variableID
	
	!Make sure that the file exists
	call CheckFileExistance(self%fileName)
	
	!Make sure that the file is open
	call CheckIfFileOpen(self)
	
	! Check if the variable you're trying to write already exists
	
	netcdfStatus = nf90_inq_varid(self%fileID, variableName, variableID)
	call CheckForNetCDFError(netcdfStatus)
	
	if (CheckVarType(self, NF90_INT, variableID)) then
		
		print *, 'WARNING: Variables not of the same type'
		print *, 'Variable Name: ', variableName
	end if
	
	!Write the variable to file
	!Make sure that the file is in data mode
	call ChangeDefineToDataMode(self)
	
	netcdfStatus =  nf90_put_var(self%fileID, variableID, arrayToWrite2D)
	call CheckForNetCDFError(netcdfStatus)
	
	
end SUBROUTINE WriteToFile2D_Int


SUBROUTINE WriteToFile2D_Dble(self, arrayToWrite2D, variableName)

	type(netcdfFile), intent(inout)					::	self 
	double precision, intent(	in)					::	arrayToWrite2D(:,:)
	character (len = *), intent(in) 		    	::	variableName
	
	integer											::	variableID
	
	!Make sure that the file exists
	
	call CheckFileExistance(self%fileName)
	
	!Make sure that the file is open
	call CheckIfFileOpen(self)
	
	
	
	! Check if the variable you're trying to write already exists
	
	netcdfStatus = nf90_inq_varid(self%fileID, variableName, variableID)
	call CheckForNetCDFError(netcdfStatus)
	
	if (CheckVarType(self, NF90_DOUBLE, variableID)) then
		
		print *, 'WARNING: Variables not of the same type'
		print *, 'Variable Name: ', variableName
	end if
	
	
	!Write the variable to file
	!Make sure that the file is in data mode
	call ChangeDefineToDataMode(self)
	
	netcdfStatus =  nf90_put_var(self%fileID, variableID, arrayToWrite2D)
	call CheckForNetCDFError(netcdfStatus)
		
end SUBROUTINE WriteToFile2D_Dble

SUBROUTINE WriteToFile2D_Dble_SingleRecord(self, arrayToWrite2D, variableName, timeStep)

	type(netcdfFile), intent(inout)					::	self 
	double precision, intent(	in)					::	arrayToWrite2D(:,:)
	character (len = *), intent(in) 		    	::	variableName
	integer,		     intent(in)					::	timeStep
	
	integer											::	variableID
	
	integer											::	countArray(3)
	integer											::	startArray(3)
	integer											::	iDim
	
	
	!Check for file existance
	call CheckFileExistance(self%fileName)
	
	!Make sure that the file is open
	call CheckIfFileOpen(self)
	
	do iDim = 1, 2
		
		startArray(iDim) = 1
		countArray(iDim) = size(arrayToWrite2D,iDim)
	end do
	
	startArray(3) = timeStep
	!print *, 'Time Step: ', timeStep
	countArray(3) = 1
	
	! Check if the variable you're trying to write already exists
	
	netcdfStatus = nf90_inq_varid(self%fileID, variableName, variableID)
	
	call CheckForNetCDFError(netcdfStatus)
	
	if (CheckVarType(self, NF90_DOUBLE, variableID)) then
		
		print *, 'WARNING: Variables not of the same type'
		print *, 'Variable Name: ', variableName
	
	end if
	
	
	!Write the variable to file
	!Make sure that the file is in data mode
	call ChangeDefineToDataMode(self)
	
	netcdfStatus =  nf90_put_var(self%fileID, variableID, arrayToWrite2D, start = startArray, count = countArray)
	call CheckForNetCDFError(netcdfStatus)

end SUBROUTINE WriteToFile2D_Dble_SingleRecord




SUBROUTINE WriteToFile3D_Dble(self, arrayToWrite3D, variableName)

	type(netcdfFile), intent(inout)					::	self 
	double precision, intent(	in)					::	arrayToWrite3D(:,:,:)
	character (len = *), intent(in) 		    	::	variableName
	
	integer											::	variableID
	
	
	!Check for file existance
	call CheckFileExistance(self%fileName)
	
	!Make sure that the file is open
	call CheckIfFileOpen(self)
	
	! Check if the variable you're trying to write already exists
	
	netcdfStatus = nf90_inq_varid(self%fileID, variableName, variableID)
	call CheckForNetCDFError(netcdfStatus)
	
	if (CheckVarType(self, NF90_DOUBLE, variableID)) then
		
		print *, 'WARNING: Variables not of the same type'
		print *, 'Variable Name: ', variableName
	
	end if
	
	
	!Write the variable to file
	!Make sure that the file is in data mode
	call ChangeDefineToDataMode(self)
	
	netcdfStatus =  nf90_put_var(self%fileID, variableID, arrayToWrite3D)
	call CheckForNetCDFError(netcdfStatus)
	

end SUBROUTINE WriteToFile3D_Dble


SUBROUTINE WriteToFile3D_Dble_SingleRecord(self, arrayToWrite3D, variableName, timeStep)

	type(netcdfFile), intent(inout)					::	self 
	double precision, intent(	in)					::	arrayToWrite3D(:,:,:)
	character (len = *), intent(in) 		    	::	variableName
	integer,		     intent(in)					::	timeStep
	
	integer											::	variableID
	
	integer											::	countArray(4)
	integer											::	startArray(4)
	integer											::	iDim
	
	
	!Check for file existance
	call CheckFileExistance(self%fileName)
	
	!Make sure that the file is open
	call CheckIfFileOpen(self)
	
	do iDim = 1, 3
		
		startArray(iDim) = 1
		countArray(iDim) = size(arrayToWrite3D,iDim)
	end do
	
	startArray(4) = timeStep
	!print *, 'Time Step: ', timeStep
	countArray(4) = 1
	
	! Check if the variable you're trying to write already exists
	
	netcdfStatus = nf90_inq_varid(self%fileID, variableName, variableID)
	
	call CheckForNetCDFError(netcdfStatus)
	
	if (CheckVarType(self, NF90_DOUBLE, variableID)) then
		
		print *, 'WARNING: Variables not of the same type'
		print *, 'Variable Name: ', variableName
	
	end if
	
	
	!Write the variable to file
	!Make sure that the file is in data mode
	call ChangeDefineToDataMode(self)
	
	netcdfStatus =  nf90_put_var(self%fileID, variableID, arrayToWrite3D, start = startArray, count = countArray)
	call CheckForNetCDFError(netcdfStatus)

end SUBROUTINE WriteToFile3D_Dble_SingleRecord

SUBROUTINE CreateDimensionPrivate(self, dimensionName, dimensionData, dimensionType)
	
	type(netcdfFile), 		intent(inout)			::	self 
	character (len = *), 	intent(	  in)			::	dimensionName
	double precision, 		intent(inout)			:: 	dimensionData(:)
	character (len = *), 	intent(	  in)			::	dimensionType		
	
	integer											::	dimensionID, variableID
	!integer											:: 	netcdfStatus
	
	
	!Make sure that the file exists
	call CheckFileExistance(self%fileName)
	
	!Make sure that the file is open
	call CheckIfFileOpen(self)
	
	!Check for the existance of the dimension
	netcdfStatus = nf90_inq_dimid(self%fileID, dimensionName, dimensionID)

	
	if(NF90_EBADDIM == netcdfStatus) then 
		
	
		self%numberOfDimensions = self%numberOfDimensions + 1
		self%dimensionNames(self%numberOfDimensions) = dimensionName
		call ChangeDataToDefineMode(self) 
		
		!Define the dimension in the file
		!!netcdfStatus = nf90_redef(self%fileID)
		
		netcdfStatus = nf90_def_dim(self%fileID, self%dimensionNames(self%numberOfDimensions), size(dimensionData), & 
									self%dimensionIDs(self%numberOfDimensions))
		
		call CheckForNetCDFError(netcdfStatus)
		
		
		call CreateNewVariable(self, self%dimensionNames(self%numberOfDimensions), self%dimensionNames(self%numberOfDimensions), dimensionType)
		call WriteDataToFile(self, dimensionData, dimensionName)
			
	
	else 
	
		call CheckForNetCDFError(netcdfStatus)
	
	end if
	
	

end SUBROUTINE CreateDimensionPrivate


SUBROUTINE CreateUnlimitedDimension(self, dimensionName)
	
	type(netcdfFile), 		intent(inout)			::	self 
	character (len = *), 	intent(	  in)			::	dimensionName
	
	integer											::	dimensionID, variableID
	!integer											:: 	netcdfStatus
	
	
	!Make sure that the file exists
	call CheckFileExistance(self%fileName)
	
	!Make sure that the file is open
	call CheckIfFileOpen(self)
	
	!Check for the existance of the dimension
	netcdfStatus = nf90_inq_dimid(self%fileID, dimensionName, dimensionID)

	
	if(NF90_EBADDIM == netcdfStatus) then 		
		
		self%numberOfDimensions = self%numberOfDimensions + 1
		self%dimensionNames(self%numberOfDimensions) = dimensionName
		call ChangeDataToDefineMode(self) 
		
		!Define the dimension in the file
		!!netcdfStatus = nf90_redef(self%fileID)
		
		netcdfStatus = nf90_def_dim(self%fileID, self%dimensionNames(self%numberOfDimensions), NF90_UNLIMITED, & 
									self%dimensionIDs(self%numberOfDimensions))
		
		
		call CheckForNetCDFError(netcdfStatus)
	
	else 
	
		call CheckForNetCDFError(netcdfStatus)
	
	end if
end SUBROUTINE CreateUnlimitedDimension

SUBROUTINE CreateSingleDimensionVariable(self, inputVariableName, inputDimensionName, variableType)
	
	type(netcdfFile), intent(inout)					::	self
	character(len=*), intent(	in)					:: 	inputVariableName
	character(len=*), intent(	in)					::	inputDimensionName
	character(len=*), intent(	in)					::	variableType
	
	
	character(len=16), dimension(1)					::	inputDimensionArray
	
	inputDimensionArray(1) = inputDimensionName
	call CreateVariablePrivate(self, inputVariableName, inputDimensionArray, variableType)


end SUBROUTINE CreateSingleDimensionVariable

SUBROUTINE CreateVariablePrivate(self, inputVariableName, inputDimensionNames, variableType)

	type(netcdfFile), intent(inout)					::	self
	character(len=*), intent(	in)					:: 	inputVariableName
	character(len=*), intent(	in)					::	inputDimensionNames(:)
	character(len=*), intent(	in)					:: 	variableType
	
	integer, pointer, dimension(:)					::	dimensionIDs
	
	integer											:: 	iDimension
	integer											:: 	variableID
	integer											::  netcdfType
	
	
	
	call CheckFileExistance(self%fileName)
	call CheckIfFileOpen(self)
	
	!Memory allocation
	allocate(dimensionIDs(size(inputDimensionNames)))
	
	!Get the dimension IDs 
	
	do iDimension=1, size(inputDimensionNames) 

		netcdfStatus = nf90_inq_dimid(self%fileID, inputDimensionNames(iDimension), dimensionIDs(iDimension))
		call CheckForNetCDFError(netcdfStatus)
		
	end do
	
	!Check to see if the variable exists 
	netcdfStatus = nf90_inq_varid(self%fileID, inputVariableName, variableID)
	netcdfType = -999
	if(NF90_ENOTVAR == netcdfStatus) then
		
		!Variable does not exist.  Create it
		
		!Get the type
		
		select case (variableType)
		
		case('int')
			netcdfType = NF90_INT
		case('integer')
			netcdfType = NF90_INT
		case('real')	
			netcdfType = NF90_FLOAT
		case('float')
			netcdfType = NF90_FLOAT
		case('double')
			netcdfType = NF90_DOUBLE
		case('dble')
			netcdfType = NF90_DOUBLE
		case('double precision')
			netcdfType = NF90_DOUBLE
		end select
		
		if(-999==netcdfType) then 
			
			print *, 'Cannot determine type: ', variableType
			stop 'Program terminates'
			
		end if 
		
		
		!Put the netcdf file in define mode
		call ChangeDataToDefineMode(self) 
		
		!Update the variable fields in the ADT
		
		self%numberOfVariables = self%numberOfVariables + 1
		self%variableNames(self%numberOfVariables) = inputVariableName
		
			
		!Call the netcdf API to define the variable
		netcdfStatus =  nf90_def_var(self%fileID, inputVariableName, netcdfType, dimensionIDs, variableID)
		call CheckForNetCDFError(netcdfStatus)
	
		self%variableIDs(self%numberOfVariables) = variableID
		
		
	else 
	
		call CheckForNetCDFError(netcdfStatus)
		
	end if
	! Change back to data mode.  
	
	call ChangeDefineToDataMode(self)
	deallocate(dimensionIDs)
	

end SUBROUTINE CreateVariablePrivate


SUBROUTINE CheckFileExistance(fileName)

	character(len=*), intent(in)					::	fileName
	logical											::	fileExists
	
	!Does the file exist? 
	inquire(FILE=fileName, EXIST=fileExists)  				 ! fileExists will be TRUE if the file exists
	if (.not.fileExists) then 
		
		print *, '======================================================'
		print *, 'File does not exist. Instantiate the netcdf object'
		print *, 'before attempting write'
		print *, '====================================================='
		
		stop     'Program terminates'
		
	end if
	
	

end SUBROUTINE CheckFileExistance

SUBROUTINE CheckIfFileOpen(self) 

	type(netcdfFile), intent(inout)						::	self 

	if(.not.(self%fileIsOpen)) then 
		
		netcdfStatus = nf90_open(self%fileName, NF90_WRITE, self%fileID)
		call CheckForNetCDFError(netcdfStatus)
		self%fileIsOpen = .true.
	
	end if

end SUBROUTINE CheckIfFileOpen

SUBROUTINE CheckForNetCDFError(netcdfStatusIn)
	
	integer, intent(in)  :: netcdfStatusIn
	
	
	if(netcdfStatus /= NF90_NOERR) then
		print *, "NetCDF ERROR!!!"
		print *, trim(nf90_strerror(netcdfStatusIn))
		stop "Stopped"
	end if
end SUBROUTINE CheckForNetCDFError


SUBROUTINE ChangeDataToDefineMode(self)  
	
	type(netcdfFile), intent(inout)					::	self 

	if(.not.(self%fileIsInDefMode)) then 
	
			netcdfStatus = nf90_redef(self%fileID)
			call CheckForNetCDFError(netcdfStatus)
			self%fileIsInDefMode=.true.
	end if


end SUBROUTINE ChangeDataToDefineMode

SUBROUTINE ChangeDefineToDataMode(self) 
	
	type(netcdfFile), intent(inout)					::	self 

	if(self%fileIsInDefMode) then 
			
			netcdfStatus = nf90_enddef(self%fileID)
			call CheckForNetCDFError(netcdfStatus)
			self%fileIsInDefMode=.false.
	end if

end SUBROUTINE ChangeDefineToDataMode

FUNCTION CheckVarType(self, varToWriteType, variableID) result(notSameType)
	
	type(netcdfFile),	intent(inout)			 	:: self
	integer,		 	intent(	  in)				:: varToWriteType
	integer, 		 	intent(inout)				:: variableID
	logical											:: notSameType 
	
	integer											:: varInFileType
	
	netcdfStatus = nf90_inquire_variable(self%fileID, variableID,  xtype=varInFileType)
	
	call CheckForNetCDFError(netcdfStatus)
	
	if(varInFileType .ne. varToWriteType) then 
		notSameType = .true.
	else 
		notSameType = .false.
	end if
	


end FUNCTION CheckVarType

end MODULE NetCDFModule
