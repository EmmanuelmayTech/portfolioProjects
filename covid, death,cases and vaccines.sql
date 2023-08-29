select * 
from projectPortfolio..covidDeaths
where continent is not null
order by 3,4

--select * 
--from projectPortfolio..covidVaccination
--order by 3,4

---lets select the data we need 
select location, date, total_cases, new_cases, total_deaths, population 
from projectPortfolio..covidDeaths
order by 1,2


---lets look at total cases vs total deaths as death percentage
---some of our columns which should be int are in nvarchar format we have to change those first
ALTER TABLE [dbo].[covidDeaths] ALTER COLUMN total_deaths decimal(18,2);
ALTER TABLE [dbo].[covidDeaths] ALTER COLUMN total_cases decimal(18,2);

select location, date, total_cases, total_deaths,  (total_deaths/total_cases)*100 as deathPercentage
from projectPortfolio..covidDeaths
order by 1,2

---lets look at this for canada
---shows the likelyhood of dying from covid
select location, date, total_cases, total_deaths,  (total_deaths/total_cases)*100 as deathPercentage
from projectPortfolio..covidDeaths
where location like'%canada%'
order by 1,2

---look into the us and china

select location, date, total_cases, total_deaths,  (total_deaths/total_cases)*100 as deathPercentage
from projectPortfolio..covidDeaths
where location like'%states%'
order by 1,2

select location, date, total_cases, total_deaths,  (total_deaths/total_cases)*100 as deathPercentage
from projectPortfolio..covidDeaths
where location like'%china%'
order by 1,2

---lets look at total cases vs population
--- shows what percentage of population has got covid

select location, date, population, total_cases,  (total_cases/population)*100 as covidPopulation
from projectPortfolio..covidDeaths
where location like'%canada%'
order by 1,2

 --- looking at countries with highest infection rate compared to population

 select location, population, max(total_cases) as HighestInfectionCount,  max((total_cases/population))*100 as covidPopulationPerc
from projectPortfolio..covidDeaths
---where location like'%canada%'
group by location, population
order by 4 desc


---lets look at countries that had highest loss of lives in respect to population
select location, population, max(total_deaths) as HighestdeathCount,  max((total_deaths/population))*100 as deathPopulationPerc
from projectPortfolio..covidDeaths
---where location like'%canada%'
group by location, population
order by 4 desc

 ---or lets just look at the numbers
 select location, max(total_deaths) as TotalDeathCount
from projectPortfolio..covidDeaths
---where location like'%canada%'
where continent is not null
group by location
order by 2 desc

---LETS BREAK THINGS DOWN BY CONTINENT (HIGHEST DEATH COUNT PER POPULATION)
select continent, max(total_deaths) as TotalDeathCount
from projectPortfolio..covidDeaths
---where location like'%canada%'
where continent is not null
group by continent
order by TotalDeathCount desc ---this however seems to omit some numbers so lets look for a way to include those null continents

--- another way to change the data type of a column use the cast FUNC or convert func eg: salary
---select cast(salary as int) from dbo.table_name ...1
---select convert(int, salary) from dbo.table_name ...2

-----select location, max(total_deaths) as TotalDeathCount
--from projectPortfolio..covidDeaths
-----where location like'%canada%'
--where continent is null
--group by location
--order by 2 desc

--- GLOBAL NUMBERS

ALTER TABLE [dbo].[covidDeaths] ALTER COLUMN new_deaths decimal(18,2);
ALTER TABLE [dbo].[covidDeaths] ALTER COLUMN new_cases decimal(18,2);


---our data seems to have some 0 denominators causing errors

select date, sum(new_cases) as totalCases, sum(new_deaths) as totalDeaths, sum(new_deaths)/nullif(sum(new_cases),0)*100 as  deathPercentage
from projectPortfolio..covidDeaths
--where location like '%states%'
where continent is not null
Group by date
order by 1,2

---lets see the total cases and total deaths as well as percentage of the world that died of covid
select sum(new_cases) as totalCases, sum(new_deaths) as totalDeaths, sum(new_deaths)/nullif(sum(new_cases),0)*100 as  deathPercentage
from projectPortfolio..covidDeaths
--where location like '%states%'
where continent is not null
---Group by date
order by 1,2

---LETS LOOK AT OUR OTHER TABLE
select*
from projectPortfolio..covidVaccination

---lets join both tables
--- looking at the total population vs vaccination

select d.continent, d.location, d.date, d.population, v.new_vaccinations
from projectPortfolio..covidDeaths d
join projectPortfolio..covidVaccination v
on d.location=v.location
and d.date=v.date
where d.continent is not null
order by 2,3

---lets do an accumulation of the newvaccinations as a rolling sum by location
alter table [dbo].[covidVaccination] alter column new_vaccinations decimal(18,2);

select d.continent, d.location, d.date, d.population, v.new_vaccinations
, sum(v.new_vaccinations) over(partition by d.location order by d.location,d.date)
as RollingVaxers
from projectPortfolio..covidDeaths d
join projectPortfolio..covidVaccination v
	on d.location=v.location
	and d.date=v.date
where d.continent is not null
order by 2,3

---NOW WE WANNA FIND NUMBER OF PEOPLE VACCINATED IN A COUNTRY
---lets create a temp table or CTE(COMMON TABLE EXPRESSIONS) :note number of columns in cte must match those in code
---1 USING A CTE
with PopvsVac ( continent, location, date, population, new_vaccinations, RollingVaxers)
as
(
select d.continent, d.location, d.date, d.population, v.new_vaccinations
, sum(v.new_vaccinations) over(partition by d.location order by d.location,d.date)
as RollingVaxers
from projectPortfolio..covidDeaths d
join projectPortfolio..covidVaccination v
	on d.location=v.location
	and d.date=v.date
where d.continent is not null
--order by 2,3
)
select *, (RollingVaxers/population)*100
from PopvsVac

---2 TEMP TABLE
CREATE TABLE #PercofVaxers
(
continent nvarchar(255),
location nvarchar (255),
date datetime,
population numeric,
new_vaccinations numeric,
RollingVaxers numeric
)

insert into #PercofVaxers
select d.continent, d.location, d.date, d.population, v.new_vaccinations
, sum(v.new_vaccinations) over(partition by d.location order by d.location,d.date)
as RollingVaxers
from projectPortfolio..covidDeaths d
join projectPortfolio..covidVaccination v
	on d.location=v.location
	and d.date=v.date
where d.continent is not null
--order by 2,3
select *, (RollingVaxers/population)*100
from #PercofVaxers

---when you need to change something in the temp table
--- it will probably give an error thus we use the drop table function
drop table if exists #PercofVaxers
CREATE TABLE #PercofVaxers
(
continent nvarchar(255),
location nvarchar (255),
date datetime,
population numeric,
new_vaccinations numeric,
RollingVaxers numeric
)

insert into #PercofVaxers
select d.continent, d.location, d.date, d.population, v.new_vaccinations
, sum(v.new_vaccinations) over(partition by d.location order by d.location,d.date)
as RollingVaxers
from projectPortfolio..covidDeaths d
join projectPortfolio..covidVaccination v
	on d.location=v.location
	and d.date=v.date
--where d.continent is not null
--order by 2,3
select *, (RollingVaxers/population)*100
from #PercofVaxers


---lets create a view to store data for later visualizations
create view PercofVaxers as 
select d.continent, d.location, d.date, d.population, v.new_vaccinations
, sum(v.new_vaccinations) over(partition by d.location order by d.location,d.date)
as RollingVaxers
from projectPortfolio..covidDeaths d
join projectPortfolio..covidVaccination v
	on d.location=v.location
	and d.date=v.date
where d.continent is not null
--order by 2,3

--- we can now query of the view cus it is now permanent until deleted

select*
from PercofVaxers


