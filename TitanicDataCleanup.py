import pandas

#Function reads in csv file containing Titanic data and cleans the data, NewFile writes cleaned data to new file 
def DataClean(File, NewFile):
    #Read in csv File
    Data = pandas.read_csv(File)

    #Create list for Ticket column values 
    DataTicket = Data['Ticket'].to_list()

    #Variable for number of rows to use when running the for loops 
    Length = len(DataTicket)

    #Changes Ticket information into similiar groups based on if they are purely numerical, or based on the lettering on beginning of ticket
    for i in range(Length):
        #If ticket is integers only change its value to 'num'
        if isinstance(DataTicket[i], int):
            DataTicket[i] = 'num'
        #If ticket includes letters and punctuation, remove all numbers and punctuation and change ticket to the remaing letters
        else:
            temp = DataTicket[i]
            temp = temp.lower()
            temp = temp[0:temp.find(' ')]
            temp = temp.replace('/', '')
            temp = temp.replace('.', '')
            DataTicket[i] = temp

    #Create a new column that contains the new values for Ticket and add it to the end of the Data dataframe
    Data.insert(loc=len(Data.columns), column='Ticket.Letters', value=DataTicket)

    #Create list for Cabin column values
    DataCabin = Data['Cabin'].tolist()
    #Create a blank list to store new Cabin values in 
    CabinTemp = []

    #Take all values in Cabin and change them to the first letter of the cabins room
    for j in range(Length):
        #If there is no value in this selection change it to None
        if pandas.isnull(DataCabin[j]):
            CabinTemp.append('None')
        #If the value is just one letter keep that value 
        elif len(DataCabin[j]) == 1:
            CabinTemp.append(DataCabin[j][0])
        #Some values have a letter followed by a space and then cabin rooms like normal, this skips the first letter and changes it to the other letter shown
        elif DataCabin[j][1] == ' ':
            CabinTemp.append(DataCabin[j][2])
        #All other values are normal room numbers, so take the first letter of room number and change it to that
        else:
            CabinTemp.append(DataCabin[j][0])

    #Create a new column that contains the new values for Cabin and add it to the end of the Data dataframe
    Data.insert(loc=len(Data.columns), column='Cabin.Letters', value=DataCabin)

    #Change Embarked location to first letter of location or Unknown if nothing is listed 
    for k in range(Length):
        if pandas.isnull(Data['Embarked'][k]):
            Data.at[k, 'Embarked'] = 'Unknown'
        elif Data['Embarked'][k] == 'Cherbourg':
            Data.at[k, 'Embarked'] = 'C'
        elif Data['Embarked'][k] == 'Southhampton':
            Data.at[k, 'Embarked'] = 'S'
        elif Data['Embarked'][k] == 'Queenstown':
            Data.at[k, 'Embarked'] = 'Q'
    #Create list for Fare column values
    DataFare = Data['Fare'].to_list()

    #Round values in Fare to two decimal places 
    for m in range(len(DataFare)):
        Data.at[m, 'Fare'] = round(Data['Fare'][m], 2).copy()

    #Save data frame changes to new csv file 
    Data.to_csv(NewFile)


