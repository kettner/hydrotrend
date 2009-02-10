/*-------------------------------------------------------------------------------------------
 *	hydroalloc_mem.c
 *	
 *	Author: Albert Kettner, March 2006
 * 
 *	Allocate memory for 1D, 2D, 3D for any variable: int, fload, double, struct, etc!
 *	Function definitions are in hydroalloc_mem.h
 * 
 *	Variable		Def.Location	Type		Units	Usage
 *	--------		------------	----		-----	-----
 *	
 *-------------------------------------------------------------------------------------------*/
   
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "hydroalloc_mem.h"
  
/*      FUNCTION ALLOCATE_1D FILE */ 
  FILE ** allocate_1d_F (int nrows)
{
  
  
  
    {
      
      
    
  



/*------------------------------------------------------
 *	...for allocating a 1D "matrix" of whatever!
 *	
 *	note that sizeof(void*) is the same as sizeof(double*)
 *	is the same as sizeof(float*), etc. A pointer always
 *	has the same size (the size of the memory address.)
 *------------------------------------------------------*/ 

matrixalloc1D (int max_width, long size) 
{
  
  
  
    {
      
      
      
    
  



/*------------------------------------------------------
 *	...for allocating a 2D matrix of whatever!
 *	
 *	note that sizeof(void*) is the same as sizeof(double*)
 *	is the same as sizeof(float*), etc. A pointer always
 *	has the same size (the size of the memory address.)
 *------------------------------------------------------*/ 

matrixalloc2D (int max_width, int max_length, long size) 
{
  
  
  
  
    {
      
      
      
    
  
    {
      
      
        {
          
          
          
        
    
  



/*------------------------------------------------------
 *	...for allocating a 3D matrix of whatever!
 *	
 *	note that sizeof(void*) is the same as sizeof(double*)
 *	is the same as sizeof(float*), etc. A pointer always
 *	has the same size (the size of the memory address.)
 *------------------------------------------------------*/ 

matrixalloc3D (int max_width, int max_length, int max_height, long size) 
{
  
  
  
  
    {
      
      
      
    
  
    {
      
      
        {
          
          
          
        
    
  
    
      {
        
        
          {
            
            
            
          
      
  


