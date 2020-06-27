package de.tuberlin.sese.cabservice.persistence.customerinteraction.dropoff;

import de.tuberlin.sese.cabservice.persistence.cab.location.CabLocationEntity;
import de.tuberlin.sese.cabservice.persistence.cab.location.CabLocationService;
import de.tuberlin.sese.cabservice.persistence.cab.registration.CabRepo;
import de.tuberlin.sese.cabservice.persistence.cab.registration.persistence.CabEntity;
import de.tuberlin.sese.cabservice.persistence.job.JobEntity;
import de.tuberlin.sese.cabservice.persistence.job.JobService;
import de.tuberlin.sese.cabservice.util.exceptions.CabCustomerPositionConflictException;
import de.tuberlin.sese.cabservice.util.exceptions.UnknownCabIdException;
import de.tuberlin.sese.cabservice.util.exceptions.UnknownCabLocationException;
import de.tuberlin.sese.cabservice.util.exceptions.UnknownJobIdException;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit4.SpringRunner;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

import static de.tuberlin.sese.cabservice.persistence.job.JobEntity.CustomerState.*;
import static java.util.Arrays.asList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.Mockito.*;

@RunWith(SpringRunner.class)
@SpringBootTest
public class DropoffServiceTest {


    @Autowired
    private DropoffService dropoffService;

    @MockBean
    private DropoffRepo dropoffRepo;

    @MockBean
    private CabRepo cabRepo;

    @MockBean
    private CabLocationService locationService;

    @MockBean
    private JobService jobService;

    @Captor
    private ArgumentCaptor<DropoffRequestEntity> dropoffRequestCaptor;

    @Captor
    private ArgumentCaptor<JobEntity> jobCaptor;

    @Test
    public void shouldDropoffCustomer() {
        when(cabRepo.findById(1L)).thenReturn(Optional.of(CabEntity.builder()
                .name("Some Cab Name")
                .id(1L)
                .build()));

        when(locationService.getCabLocation(1L)).thenReturn(Optional.of(CabLocationEntity.builder()
                .cabId(1L)
                .section(2)
                .build()));

        when(jobService.getJob(3L)).thenReturn(Optional.of(JobEntity.builder()
                .id(3L)
                .start(11)
                .end(2)
                .timestamp(LocalDateTime.of(2020, 6, 27, 7, 48))
                .customerId(3L)
                .customerState(IN_CAB)
                .inProgress(true)
                .build()));

        dropoffService.dropoff(1L, 3L);

        verify(cabRepo).findById(1L);
        verify(locationService).getCabLocation(1L);
        verify(jobService).getJob(3L);
        verify(dropoffRepo).save(dropoffRequestCaptor.capture());

        DropoffRequestEntity dropoffCaptorValue = dropoffRequestCaptor.getValue();
        assertThat(dropoffCaptorValue.getCabId()).isEqualTo(1L);
        assertThat(dropoffCaptorValue.getCustomerId()).isEqualTo(3L);

        verifyNoMoreInteractions(cabRepo);
        verifyNoMoreInteractions(locationService);
        verifyNoMoreInteractions(jobService);
        verifyNoMoreInteractions(dropoffRepo);
    }

    @Test
    public void shouldThrowIllegalArgumentExceptionOnMissingCabIdDuringDropoff() {
        assertThatThrownBy(() -> dropoffService.dropoff(null, 3L))
                .isExactlyInstanceOf(IllegalArgumentException.class);
    }

    @Test
    public void shouldThrowIllegalArgumentExceptionOnMissingCustomerIdDuringDropoff() {
        assertThatThrownBy(() -> dropoffService.dropoff(1L, null))
                .isExactlyInstanceOf(IllegalArgumentException.class);
    }

    @Test
    public void shouldThrowUnknownCabIdExceptionOnUnknownCabIdDuringDropoff() {
        when(cabRepo.findById(1L)).thenReturn(Optional.empty());

        assertThatThrownBy(() -> dropoffService.dropoff(1L, 3L))
                .isExactlyInstanceOf(UnknownCabIdException.class);
    }

    @Test
    public void shouldThrowUnknownCabLocationExceptionOnUnknownCabLocationDuringDropoff() {
        when(cabRepo.findById(1L)).thenReturn(Optional.of(CabEntity.builder()
                .name("Some Cab Name")
                .id(1L)
                .build()));

        when(locationService.getCabLocation(1L)).thenReturn(Optional.empty());

        assertThatThrownBy(() -> dropoffService.dropoff(1L, 3L))
                .isExactlyInstanceOf(UnknownCabLocationException.class);
    }

    @Test
    public void shouldThrowUnknownJobIdExceptionOnUnknownJobIdDuringDropoff() {
        when(cabRepo.findById(1L)).thenReturn(Optional.of(CabEntity.builder()
                .name("Some Cab Name")
                .id(1L)
                .build()));

        when(locationService.getCabLocation(1L)).thenReturn(Optional.of(CabLocationEntity.builder()
                .cabId(1L)
                .section(2)
                .build()));

        when(jobService.getJob(3L)).thenReturn(Optional.empty());

        assertThatThrownBy(() -> dropoffService.dropoff(1L, 3L))
                .isExactlyInstanceOf(UnknownJobIdException.class);
    }

    @Test
    public void shouldThrowCabCustomerPositionConflictExceptionWhenCabNotAtCustomerTargetStationDuringDropoff() {
        when(cabRepo.findById(1L)).thenReturn(Optional.of(CabEntity.builder()
                .name("Some Cab Name")
                .id(1L)
                .build()));

        when(locationService.getCabLocation(1L)).thenReturn(Optional.of(CabLocationEntity.builder()
                .cabId(1L)
                .section(5)
                .build()));

        when(jobService.getJob(3L)).thenReturn(Optional.of(JobEntity.builder()
                .id(3L)
                .start(5)
                .end(11)
                .timestamp(LocalDateTime.of(2020, 6, 27, 7, 48))
                .customerId(3L)
                .customerState(WAITING)
                .inProgress(true)
                .build()));

        assertThatThrownBy(() -> dropoffService.dropoff(1L, 3L))
                .isExactlyInstanceOf(CabCustomerPositionConflictException.class);
    }

    @Test
    public void shouldThrowCabCustomerPositionConflictExceptionWhenCustomerWasNotPickedUpYetDuringDropoff() {
        when(cabRepo.findById(1L)).thenReturn(Optional.of(CabEntity.builder()
                .name("Some Cab Name")
                .id(1L)
                .build()));

        when(locationService.getCabLocation(1L)).thenReturn(Optional.of(CabLocationEntity.builder()
                .cabId(1L)
                .section(2)
                .build()));

        when(jobService.getJob(3L)).thenReturn(Optional.of(JobEntity.builder()
                .id(3L)
                .start(2)
                .end(5)
                .timestamp(LocalDateTime.of(2020, 6, 27, 7, 48))
                .customerId(3L)
                .customerState(WAITING)
                .inProgress(true)
                .build()));

        assertThatThrownBy(() -> dropoffService.dropoff(1L, 3L))
                .isExactlyInstanceOf(CabCustomerPositionConflictException.class);
    }

    @Test
    public void shouldThrowCabCustomerPositionConflictExceptionWhenCustomerWasAlreadyDroppedOffDuringDropoff() {
        when(cabRepo.findById(1L)).thenReturn(Optional.of(CabEntity.builder()
                .name("Some Cab Name")
                .id(1L)
                .build()));

        when(locationService.getCabLocation(1L)).thenReturn(Optional.of(CabLocationEntity.builder()
                .cabId(1L)
                .section(2)
                .build()));

        when(jobService.getJob(3L)).thenReturn(Optional.of(JobEntity.builder()
                .id(3L)
                .start(2)
                .end(5)
                .timestamp(LocalDateTime.of(2020, 6, 27, 7, 48))
                .customerId(3L)
                .customerState(AT_DESTINATION)
                .inProgress(true)
                .build()));

        assertThatThrownBy(() -> dropoffService.dropoff(1L, 3L))
                .isExactlyInstanceOf(CabCustomerPositionConflictException.class);
    }

    @Test
    public void shouldReturnListWhenGettingDropoffRequests() {
        when(dropoffRepo.findAll()).thenReturn(asList(
                DropoffRequestEntity.builder()
                        .cabId(1L)
                        .customerId(2L)
                        .build(),
                DropoffRequestEntity.builder()
                        .cabId(2L)
                        .customerId(1L)
                        .build()));

        List<DropoffRequestEntity> dropoffRequests = dropoffService.getDropoffRequests();

        assertThat(dropoffRequests).hasSize(2);
        assertThat(dropoffRequests).isInstanceOf(List.class);
    }

    @Test
    public void shouldReturnCompleteOnNoActiveDropoffsDuringDropoffsComplete() {
        when(cabRepo.findById(1L)).thenReturn(Optional.of(CabEntity.builder()
                .id(1L)
                .name("Some Name")
                .build()));

        when(dropoffRepo.findAll()).thenReturn(asList(
                DropoffRequestEntity.builder()
                        .cabId(2L)
                        .customerId(1L)
                        .build(),
                DropoffRequestEntity.builder()
                        .cabId(3L)
                        .customerId(2L)
                        .build()));

        DropoffCompleteModel dropoffCompleteModel = dropoffService.dropoffsComplete(1L);

        assertThat(dropoffCompleteModel).isNotNull();
        assertThat(dropoffCompleteModel.isComplete()).isTrue();

        verify(cabRepo).findById(1L);
        verify(dropoffRepo).findAll();

        verifyNoMoreInteractions(cabRepo);
        verifyNoMoreInteractions(dropoffRepo);
    }

    @Test
    public void shouldReturnNotCompleteOnOnlyActiveDropoffsDuringDropoffsComplete() {
        when(cabRepo.findById(1L)).thenReturn(Optional.of(CabEntity.builder()
                .id(1L)
                .name("Some Name")
                .build()));

        when(dropoffRepo.findAll()).thenReturn(asList(
                DropoffRequestEntity.builder()
                        .cabId(1L)
                        .customerId(1L)
                        .build(),
                DropoffRequestEntity.builder()
                        .cabId(1L)
                        .customerId(2L)
                        .build()));

        DropoffCompleteModel dropoffCompleteModel = dropoffService.dropoffsComplete(1L);

        assertThat(dropoffCompleteModel).isNotNull();
        assertThat(dropoffCompleteModel.isComplete()).isFalse();

        verify(cabRepo).findById(1L);
        verify(dropoffRepo).findAll();

        verifyNoMoreInteractions(cabRepo);
        verifyNoMoreInteractions(dropoffRepo);
    }

    @Test
    public void shouldReturnNotCompleteOnSomeActiveDropoffsDuringDropoffsComplete() {
        when(cabRepo.findById(1L)).thenReturn(Optional.of(CabEntity.builder()
                .id(1L)
                .name("Some Name")
                .build()));

        when(dropoffRepo.findAll()).thenReturn(asList(
                DropoffRequestEntity.builder()
                        .cabId(1L)
                        .customerId(1L)
                        .build(),
                DropoffRequestEntity.builder()
                        .cabId(2L)
                        .customerId(2L)
                        .build(),
                DropoffRequestEntity.builder()
                        .cabId(3L)
                        .customerId(4L)
                        .build()));

        DropoffCompleteModel dropoffCompleteModel = dropoffService.dropoffsComplete(1L);

        assertThat(dropoffCompleteModel).isNotNull();
        assertThat(dropoffCompleteModel.isComplete()).isFalse();

        verify(cabRepo).findById(1L);
        verify(dropoffRepo).findAll();

        verifyNoMoreInteractions(cabRepo);
        verifyNoMoreInteractions(dropoffRepo);
    }

    @Test
    public void shouldThrowIllegalArgumentExceptionWhenCabIdIsMissingOnDropoffsComplete() {
        assertThatThrownBy(() -> dropoffService.dropoffsComplete(null))
                .isExactlyInstanceOf(IllegalArgumentException.class);
    }

    @Test
    public void shouldThrowUnknownCabIdExceptionWhenCabIdIsUnknownOnDropoffsComplete() {
        when(cabRepo.findById(1L)).thenReturn(Optional.empty());

        assertThatThrownBy(() -> dropoffService.dropoffsComplete(1L))
                .isExactlyInstanceOf(UnknownCabIdException.class);
    }

    @Test
    public void shouldAcceptDropoff() {
        when(jobService.getJob(1L)).thenReturn(Optional.of(JobEntity.builder()
                .id(1L)
                .customerState(IN_CAB)
                .build()));

        dropoffService.acceptDropoff(1L);

        verify(jobService).getJob(1L);
        verify(jobService).updateJob(jobCaptor.capture());
        assertThat(jobCaptor.getValue().getCustomerState()).isEqualTo(AT_DESTINATION);
        verify(dropoffRepo).deleteById(1L);

        verifyNoMoreInteractions(jobService);
        verifyNoMoreInteractions(dropoffRepo);
    }

    @Test
    public void shouldThrowIllegalArgumentExceptionOnMissingCustomerIdDuringAcceptDropoff() {
        assertThatThrownBy(() -> dropoffService.acceptDropoff(null))
                .isExactlyInstanceOf(IllegalArgumentException.class);
    }

    @Test
    public void shouldThrowUnknownJobIdExceptionOnUnknownJobIdDuringAcceptDropoff() {
        when(jobService.getJob(1L)).thenReturn(Optional.empty());

        assertThatThrownBy(() -> dropoffService.acceptDropoff(1L))
                .isExactlyInstanceOf(UnknownJobIdException.class);
    }
}
