use PortfolioProject;

--select SaleDate, CONVERT(date, SaleDate)
--from housing;

--update housing
--set SaleDate = CONVERT(date, SaleDate);

--select * from housing;

-- Convert 'SalaDate' into Date Format
alter table housing
add SaleDateConverted Date

go

update housing
set SaleDateConverted = CONVERT(date, SaleDate)

go

select * from housing;

-- Populate PropertyAddress

select *
from housing
where ParcelID = '025 07 0 031.00';

select h1.PropertyAddress, h2.PropertyAddress, ISNULL(h1.PropertyAddress, h2.PropertyAddress)
from housing h1
join housing h2
on h1.ParcelID = h2.ParcelID and h1.[UniqueID ] <> h2.[UniqueID ]
where h1.PropertyAddress is null

update h1
set PropertyAddress = ISNULL(h1.PropertyAddress, h2.PropertyAddress)
from housing h1
join housing h2
on h1.ParcelID = h2.ParcelID and h1.[UniqueID ] <> h2.[UniqueID ]
where h1.PropertyAddress is null

select * from housing

-- Spliting PropertyAddress into address and city

select PropertyAddress
from housing

select SUBSTRING(PropertyAddress, 1, CHARINDEX(',', PropertyAddress)-1),
		SUBSTRING(PropertyAddress, CHARINDEX(',', PropertyAddress)+1, len(PropertyAddress))

alter table housing
add PropertySplitAddress nvarchar(255),
	PropertySplitCity    nvarchar(255)

go

update housing
set PropertySplitAddress = SUBSTRING(PropertyAddress, 1, CHARINDEX(',', PropertyAddress)-1),
	PropertySplitCity = SUBSTRING(PropertyAddress, CHARINDEX(',', PropertyAddress)+1, len(PropertyAddress)) 

go

-- Spliting OwnerAddress into address, city and state

select PARSENAME(replace(OwnerAddress,',', '.'), 3)
from housing

alter table housing
add OwnerSplitAddress nvarchar(255),
	OwnerSplitCity nvarchar(255),
	OwnerSplitState nvarchar(255)

go

Update housing
set OwnerSplitAddress = PARSENAME(replace(OwnerAddress,',', '.'), 3),
	OwnerSplitCity = PARSENAME(replace(OwnerAddress,',', '.'), 2),
	OwnerSplitState = PARSENAME(replace(OwnerAddress,',', '.'), 1)

go

select OwnerAddress, count(*) 
from housing
group by OwnerAddress
having count(*) > 1

select h1.OwnerAddress, h2.OwnerAddress
from housing h1
join housing h2
on h1.ParcelID = h2.ParcelID and h1.[UniqueID ] <> h2.[UniqueID ];

-- Replace 'Y'/'N' with 'Yes'/'No' in 'SoldAsVacant' field

select SoldAsVacant, count(*)
from housing
group by SoldAsVacant
order by 2 desc

select SAV, count(SAV)
from 
	(select SoldAsVacant,
			case when SoldAsVacant = 'Y' then 'Yes'
				 when SoldAsVacant = 'N' then 'No'
				 else SoldAsVacant
				 end as SAV
	from housing) h
group by SAV
order by 2 desc

update housing
set SoldAsVacant = case when SoldAsVacant = 'Y' then 'Yes'
					when SoldAsVacant = 'N' then 'No'
					else SoldAsVacant
					end


-- Detect duplicates and remove them

with rownumCTE as
(select *,
		row_number() over (partition by ParcelID, 
										PropertyAddress,
										SaleDate,
										SalePrice,
										LegalReference
							order by uniqueID) as row_num
from housing)
select *
from rownumCTE
where row_num > 1

select *
from housing
where ParcelID = '081 02 0 144.00'