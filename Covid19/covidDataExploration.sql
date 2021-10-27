use PortfolioProject

select *
from covid_deaths
where continent is not null
order by 3, 4

-- Select data to be used in the beginning

select location, date, population, total_cases, new_cases, total_deaths
from covid_deaths
where continent is not null
order by 1,2

-- ...|Total_cases|Total_deaths|Likelihood of dying if get infected

select location, date, total_deaths, total_cases, 
		round((total_deaths/total_cases)*100,2) as deathPercentage
from covid_deaths
where continent is not null and location = 'Denmark'
order by 1,2

-- ...|Total_cases|Population

select location, date,
		round((total_cases/population)*100, 2) as PopulationInfectedPercentage
from covid_deaths
where continent is not null and location = 'Denmark'
order by 1,2

select location,
		max(total_cases) as HighestInfectionCount,
		max(round((total_cases/population)*100, 2)) as HighestInfectionRate
from covid_deaths
group by location
order by 3 desc

select location, 
		total_cases as InfectionCount, 
		round((total_cases/population)*100, 2) as InfectionRate
from
	(select *,
			rank() over (partition by location order by total_cases desc) as rank_count,
			rank() over (partition by location order by round((total_cases/population)*100, 2) desc) as rank_rate
	 from covid_deaths) as c
where rank_count=1 or rank_rate=1
order by 3 desc


-- ...|Total_deaths|Population

alter table covid_deaths
alter column total_deaths int

select location,
		max(total_deaths) as deathCount
from covid_deaths
where continent is not null
group by location
order by 2 desc

-- Continentwise analysis or breaking things down by continent
-- Highest death count

select location, max(total_deaths) as deathCount
from covid_deaths
where continent is null
group by location
order by 2 desc


select continent, max(total_deaths) as deathCount
from covid_deaths
where continent is not null
group by continent
order by 2 desc

select continent, location, total_deaths as deathCount
from
	(select *,
			rank() over (partition by continent order by total_deaths desc) rank_death
	 from covid_deaths
	 where continent is not null) c
where rank_death = 1
order by 3 desc

-- Global Numbers
alter table covid_deaths
alter column new_deaths int


select sum(new_cases) as total_cases,
		sum(new_deaths) as total_deaths
from covid_deaths
where continent is not null

select sum(new_cases), sum(new_deaths) from covid_deaths where location = 'World'
select max(total_cases), max(total_deaths) from covid_deaths where location = 'World'


-- Population|Vaccination
alter table covid_vaccinations
alter column new_vaccinations bigint;

--alter table covid_vaccinations
--alter column total_vaccinations int

select vac.continent, vac.location, vac.date, dea.population, vac.new_vaccinations,
		sum(vac.new_vaccinations) over (partition by vac.location order by vac.date) as cummulativeVaccination
from covid_vaccinations vac
join covid_deaths dea
on vac.location = dea.location and vac.date = dea.date
where vac.location = 'Denmark'
order by 2, 3

with PopVsVacCTE as
(
select vac.continent, vac.location, vac.date, dea.population, vac.new_vaccinations,
		sum(vac.new_vaccinations) over (partition by vac.location order by vac.date) as cummulativeVaccination
from covid_vaccinations vac
join covid_deaths dea
on vac.location = dea.location and vac.date = dea.date
where vac.continent is not null 
--order by 2, 3
)
select *, round((cummulativeVaccination/population)*100,2) from PopVsVacCTE where location = 'Denmark'

-- Creating Temp Table to perform calculation on the right above query

drop table if exists VacRate
create table VacRate
(
continent nvarchar(255),
location nvarchar(255),
date datetime,
population numeric,
new_vaccinations numeric,
cummulativeVaccination numeric
)

go

insert into VacRate
select vac.continent, vac.location, vac.date, dea.population, vac.new_vaccinations,
		sum(vac.new_vaccinations) over (partition by vac.location order by vac.date) as cummulativeVaccination
from covid_vaccinations vac
join covid_deaths dea
on vac.location = dea.location and vac.date = dea.date

go 

select *, round((cummulativeVaccination/population),2)*100 from VacRate where location = 'Denmark'

go
-- Creating View to store results for later viz

create view VacRate_view as
select vac.continent, vac.location, vac.date, dea.population, vac.new_vaccinations,
		sum(vac.new_vaccinations) over (partition by vac.location order by vac.date) as cummulativeVaccination
from covid_vaccinations vac
join covid_deaths dea
on vac.location = dea.location and vac.date = dea.date
where vac.continent is not null








--select location, date, total_cases, total_deaths,
--	round(total_deaths / total_cases * 100, 2) as death_rate
--from covid_deaths
--where location = 'Denmark'
--order by 2;

--select location, 
--	max(total_cases) as total_case
--from covid_deaths 
--where continent is null
--group by location;

--select continent, 
--	sum(new_cases) as total_case
--from covid_deaths 
--where continent is not null
--group by continent;

--select location, 
--	-sum(new_cases) as total_corrected_case
--from covid_deaths 
--where continent is not null and new_cases < 0
--group by location
--order by 2 desc;

--select location, population
--from (select *,
--			rank() over (partition by location order by population) as rank_pop
--		from covid_deaths
--		where continent is null and population is not null) cd
--where rank_pop = 1;

--select distinct location, population
--from covid_deaths
--where continent is null and population is not null;

--select *,
--		round((total_deaths/total_population) *100, 4) death_rate
--from
--	(select location,
--			max(total_deaths) as total_deaths,
--			max(population) as total_population
--	from covid_deaths
--	where continent is null and population is not null
--	group by location) as cd
--order by death_rate desc;

Select SUM(new_cases) as total_cases, 
	   SUM(cast(new_deaths as int)) as total_deaths,
	   SUM(cast(new_deaths as int))/SUM(New_Cases)*100 as DeathPercentage
From covid_deaths
where continent is not null 
order by 1,2

Select location, SUM(cast(new_deaths as int)) as TotalDeathCount
From covid_deaths
Where continent is null 
and location not in ('World', 'European Union', 'International')
Group by location
order by TotalDeathCount desc

Select Location, Population,
	   MAX(total_cases) as HighestInfectionCount,
	   Max((total_cases/population))*100 as PercentPopulationInfected
From covid_deaths
Group by Location, Population
order by PercentPopulationInfected desc

Select Location, Population,date, 
	   MAX(total_cases) as HighestInfectionCount,  
	   Max((total_cases/population))*100 as PercentPopulationInfected
From covid_deaths
Group by Location, Population, date
order by PercentPopulationInfected desc