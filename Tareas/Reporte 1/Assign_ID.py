#Importing used libraries
import pandas as pd
import numpy as np

#Making a function to automatically assign a number ID to unique column values
def Column_To_ID (df, column, idx): #Defining function name and parameters (Dataframe(df) and column of the dataframe to assign ID(str))
    ID_Start = 1 #Defines where the ID number starts in my case 1
    Unique_Values = df[column].unique() #Finds unique values in the column
    
    #Adding columns to clearly show the Unique value and its ID
    idx[column + '_index'] = np.nan #Creates a column to save the unique values
    idx[column + '_id'] = np.nan #Creates a column to save the ID
    
    #For loop that replaces the Values in the column with their new ID
    for i in Unique_Values:
        df[column].replace(i, ID_Start, inplace = True) #Replaces Column value with its ID
        idx.loc[ID_Start - 1, column + '_id'] = ID_Start #Adds the id to the created column for ids
        ID_Start += 1 #Changes the ID for the next unique value
        
    idx[column + '_index'] = pd.Series(Unique_Values) #Places all the unique values in the appropiate column
        
    print(idx[[column + '_index', column + '_id']].head(ID_Start - 1))#Prints the index of the values with their IDs
    print('-------------------------------------------------------------------------')

pd.options.display.max_columns = None #Allows to see all the columns of the data frame

Watches = pd.read_csv('C:/Users/alvar/Downloads/Luxury watch.csv') #Reads the file with the data

print(Watches.head) #I print it to see the data and their values

IDX = pd.DataFrame() #Creation of a data frame to save the unique values and their id by columns

#Using the created function to assign IDs
Column_To_ID(Watches, 'Case Material', IDX)
Column_To_ID(Watches, 'Strap Material', IDX)
Column_To_ID(Watches, 'Movement Type', IDX)
Column_To_ID(Watches, 'Crystal Material', IDX)
Column_To_ID(Watches, 'Complications', IDX)

#Removing meaurement units of the values to just have numerical values.
Watches['Water Resistance'] = Watches['Water Resistance'].str.rstrip(' meters')
Watches['Power Reserve'] = Watches['Power Reserve'].str.rstrip(' hours')

#Saving the modified dataframe to a new csv file.
#Watches.to_csv(r'C:/Users/alvar/Downloads/Luxury Watches (With Ids).csv', sep=',', index=False, encoding='utf-8')

#IDX.to_csv(r'C:/Users/alvar/Downloads/Luxury Watches Ids.csv', sep=',', index=False, encoding='utf-8')